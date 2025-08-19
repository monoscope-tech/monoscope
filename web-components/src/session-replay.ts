import { html, LitElement, nothing, PropertyValues } from 'lit';
import { customElement, property, query, state } from 'lit/decorators.js';
import { EventType, eventWithTime } from '@rrweb/types';
import { faSprite_ } from './monitors/test-editor-utils';
import { ConsoleEvent } from './types/types';
import { Replayer } from '@rrweb/replay';

const MS_10 = 10000;
@customElement('session-replay')
export class SessionReplay extends LitElement {
  @property({ type: String }) private projectId: string = '';
  @property({ type: String }) private containerId: String = '';

  @state() private activityWidth = 0;
  @query('#replayerOuterContainer') private replayerOuterContainer: HTMLElement;
  @query('#progressBar') private progressBar: HTMLElement;
  @state() private playSpeed = 1;
  @state() private skipInactive = true;
  @state() private consoleEventsEnable = [true, true, true]; // error, warn, info;
  @state() private paused = false;
  @state() private isLoading = false;

  @state() private consoleEvents: ConsoleEvent[] = [];
  @state() private currentEventTime: number = 0;
  @state() private currentTime = 0;
  @state() private finished = false;
  @state() private syncScrolling = true;
  @state() private trickTarget = 0;

  private startX: number | null = null;
  private player: Replayer | null = null;
  private events: eventWithTime[] = [];
  private containerWidth = 1024;
  private timeout: any = null;

  private containerHeight = 550;
  private iframeWidth = 1117;
  private trickPlayer: Replayer | null = null;
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
    this.fetchNewSessionData = this.fetchNewSessionData.bind(this);
    this.initiatePlayer = this.initiatePlayer.bind(this);
    this.handleTimeSeek = this.handleTimeSeek.bind(this);
    this.closePlayerWindow = this.closePlayerWindow.bind(this);
    document.addEventListener('mousemove', (e) => {
      if (this.startX !== null) {
        const diff = this.startX - e.clientX;
        const curr = this.activityWidth + diff;
        this.activityWidth = curr;
        this.startX = e.clientX;
      }
    });

    window.addEventListener('loadSessionReplay', (e) => {
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

  debounce(fn :()=>void) {
    if(this.timeout) {
      clearTimeout(this.timeout)
    }
    this.timeout = setTimeout(()=> fn(),10)
  }



  updateContainerWidths() {
    const frameContainer = document.querySelector('.player-frame') as HTMLElement;
    frameContainer.style.width = `${this.containerWidth}px`;
    frameContainer.style.height = `${this.containerHeight}px`;
  }
  updateScale = () => {
    this.updateContainerWidths();
    const el = this.player?.wrapper;
    const widthScale = (this.containerWidth) / this.iframeWidth;
    const heightScale = this.containerHeight / this.iframeHeight;
    if (el) {
      el.style.transform = `scale(${Math.min(widthScale, heightScale)}) translate(-50%, -50%)`;
    }
  };

  protected updated(changedProperties: PropertyValues): void {
    if (this.player) {

      if (changedProperties.has('activityWidth')) {
        this.debounce(()=> {
        const mContainer = Number(getComputedStyle(this.replayerOuterContainer).width.replace('px', ''));
        this.containerWidth = mContainer - this.activityWidth;
        this.updateScale();
        })
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
          document.querySelector('#a-' + this.currentEventTime)?.scrollIntoView();
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

  async initiatePlayer(events: eventWithTime[]) {
    if (events.length < 2) return;
    this.events = events;
    const target = document.querySelector('#playerWrapper') as HTMLElement;
    this.currentTime = 0;
    this.consoleEvents = [];
    events.forEach((event) => {
      if (event.type === EventType.Plugin && event.data.plugin === 'rrweb/console@1') {
        const level = (event as ConsoleEvent).data.payload.level;
        this.consoleTypesCounts[level] += 1;
        this.consoleEvents = [...this.consoleEvents, event as ConsoleEvent];
      }
    });

    this.player = new Replayer(events, { root: target, plugins: [{ handler: this.handleConsoleEvents }], skipInactive: this.skipInactive });
    this.metaData = this.player.getMetaData();
    this.updateScale();
    this.play();
    this.observer.disconnect();
    this.observer.observe(this.player.iframe, { attributes: true, attributeFilter: ['width', 'height'] });
  }

  fetchNewSessionData(sessionId: string) {
    if (this.isLoading) return;
    this.pause();
    try {
      this.player?.destroy();
    } catch (error) {}
    this.isLoading = true;
    const url = `/p/${this.projectId}/replay_session/${sessionId}`;

    fetch(url, { method: 'GET', headers: { Accept: 'application/json' } })
      .then((response) => response.json())
      .then((data) => {
        this.initiatePlayer(data.events);
      })
      .finally(() => {
        this.isLoading = false;
      });
  }

  protected firstUpdated(_changedProperties: PropertyValues): void {
    const mContainer = Number(getComputedStyle(this.replayerOuterContainer).width.replace('px', ''));
    this.containerWidth = mContainer - this.activityWidth;
    const events = JSON.parse(localStorage.getItem('qq') || '[]').events;
    this.initiatePlayer(events);

    const container = this.renderRoot.querySelector<HTMLDivElement>('#replayerOuterContainer');

    if (container)  {
    const resizeObserver = new ResizeObserver((entries) => {
      this.debounce(()=> {
      for (let entry of entries) {
        const { width, height } = entry.contentRect;
        const comp = getComputedStyle(this.replayerOuterContainer);
        const mContainer = Number(comp.width.replace('px', ''));
        const mHeight = Number(comp.height.replace("px",""))
        this.containerWidth = mContainer - this.activityWidth;
        this.containerHeight = mHeight - 120

        this.updateScale();
      }
      })
    });

    // Start observing
    resizeObserver.observe(container);
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
    this.player?.destroy();
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
        bgColor = 'bg-red-100 dark:bg-red-900';
        textColor = 'text-textError';
        hoverColor = 'hover:bg-fillError-weak';
        break;
      case 'warn':
        bgColor = 'bg-yellow-100';
        textColor = 'text-textWarn';
        hoverColor = 'hover:bg-fillWarn-weak';
        break;
      case 'info':
        bgColor = 'bg-fillBrand-weak dark:bg-blue-900';
        textColor = 'text-textInfo';
        hoverColor = 'hover:bg-fillBrand-weak';
        break;
      default:
        return html`...`;
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
            ${faSprite_('chevron-up', 'regular', 'w-2.5 h-2.5 dark:fill-gray-200')}
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
    return html`<div
      class="flex overflow-x-hidden resize w-max"
      id="replayerOuterContainer"
      style="height:${this.containerHeight + 120}px; width:1024px"
    >
      <div class="w-full  flex flex-col justify-start shrink-1 min-w-0 overflow-hidden">
        <div class="bg-fillWeak w-full px-2 h-10 min-h-10 flex items-center border-b gap-4 cursor-move  justify-between playerHeader">
          <div class="flex items-center gap-4 shrink-1">
            <h3 class="font-medium h-full truncate overflow-ellipsis min-w-0">Session recording</h3>
          </div>

          <div class="flex items-center gap-4 text-xs font-semibold">
            <div class="dropdown">
              <div tabindex="0" role="button" class="cursor-pointer flex items-center gap-1 ${this.playSpeed != 1 ? 'text-textBrand' : ''}">
                ${faSprite_('gauge', 'regular', 'w-3 h-3')} Speed ${this.playSpeed}x
              </div>
              <ul tabindex="0" class="dropdown-content menu bg-base-100 border text-xs rounded-box z-1 w-max p-2 shadow">
                ${[0.5, 1, 1.5, 2, 3, 4, 8, 16].map(
                  (s) =>
                    html`<li>
                      <button
                        class="px-4 rounded py-1 hover:bg-fillWeak ${this.playSpeed == s ? 'bg-fillBrand-strong text-white' : ''}"
                        @click=${() => (this.playSpeed = s)}
                      >
                        ${s}x
                      </button>
                    </li>`
                )}
              </ul>
            </div>
            <button
              class="flex items-center cursor-pointer gap-1 ${this.skipInactive ? 'text-textBrand' : ''}"
              @click=${() => (this.skipInactive = !this.skipInactive)}
            >
              ${faSprite_('skip', 'regular', 'w-3 h-3')}
              <span>Skip inactive</span>
            </button>
            <button
              @click=${() => {
                this.activityWidth = this.activityWidth <= 0 ? 300 : 0;
              }}
              class="cursor-pointer flex items-center gap-1 ${this.activityWidth > 0 ? 'text-textBrand' : ''}"
            >
              ${faSprite_('side-chevron-left-in-box', 'regular', 'w-3 h-3')} Activity
            </button>
            ${this.activityWidth === 0
              ? html`<button class=" hover:bg-fillWeak" @click=${this.closePlayerWindow}>
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
              class="absolute inset-0 flex bg-black opacity-25 items-center  justify-center ${this.paused || this.finished || this.isLoading
                ? ''
                : 'hidden'}"
            >
              ${this.isLoading
                ? html`<div class="italic text-7xl font-medium text-gray-100">Loading...</div>`
                : this.finished
                ? html`
                    <button @click=${() => this.goTo(0)} class="cursor-pointer">
                      ${faSprite_('replay', 'regular', 'w-14 h-14 text-gray-100')}
                    </button>
                  `
                : html`
                    <button @click=${() => (this.paused = false)} class="cursor-pointer">
                      ${faSprite_('p-play', 'regular', 'w-14 h-14 text-gray-100')}
                    </button>
                  `}
            </div>
          </div>
          <div class="flex flex-col items-center w-full py-4 h-20">
            <div
              id="progressBar"
              @click=${this.handleTimeSeek}
              @mouseover=${this.handleTrickPlay}
              @mousemove=${this.handleTrickPlay}
              @mouseleave=${() => {
                this.trickPlayer?.pause();
              }}
              class="relative progress-container h-1.5 cursor-pointer rounded group bg-gray-200"
              style="width:calc(100% - 32px)"
            >
              <div
                class="bg-slate-800 absolute text-sm font-medium rounded -translate-x-1/2 overflow-hidden border border-slate-800 hidden group-hover:block"
                style="left:${(this.trickTarget / this.metaData.totalTime) * 100}%; top:-200px"
              >
                <div id="trickPlayerContainer" class="h-40 w-60 bg-black overflow-hidden"></div>
                <div class="text-gray-100 text-center w-full py-2">${SessionReplay.formatTime(this.trickTarget)}</div>
              </div>
              <div class="relative h-full bg-fillBrand-strong" style="width:${(this.currentTime / this.metaData.totalTime) * 100}%">
                <span class="absolute right-0 h-4 w-4 top-1/2 -translate-y-1/2 rounded-full bg-fillBrand-strong"></span>
              </div>
            </div>
            <div class="flex p-4 w-full items-center justify-between">
              <div>
                <div class="text-xs flex gap-0.5 font-mono font-medium flex-nowrap w-max">
                  <span>${SessionReplay.formatTime(this.currentTime)}</span>
                  <span>/</span>
                  <span>${SessionReplay.formatTime(this.metaData.totalTime)}</span>
                </div>
              </div>
              <div class="w-full gap-3 flex items-center justify-center">
                <button class="relative cursor-pointer" @click=${() => this.goTo(this.currentTime - MS_10)}>
                  <span style="font-size:8px" class="absolute top-1/2  left-1/2 -translate-x-1/2 -translate-y-1/2 font-semibold">10</span>
                  ${faSprite_('time-skip', 'regular', 'h-5 w-5')}
                </button>
                ${this.finished
                  ? html`<button class="flex justify-center cursor-pointer items-center" @click=${() => this.goTo(0)}>
                      ${faSprite_('replay', 'regular', 'h-5 w-5')}
                    </button>`
                  : html`
                      <button class="flex justify-center cursor-pointer items-center" @click=${() => (this.paused = !this.paused)}>
                        ${this.paused ? faSprite_('p-play', 'regular', 'h-5 w-5') : faSprite_('p-pause', 'regular', 'h-5 w-5')}
                      </button>
                    `}
                <button class="relative cursor-pointer" @click=${() => this.goTo(this.currentTime + MS_10)}>
                  <span style="font-size: 8px" class="absolute top-1/2  left-1/2 -translate-x-1/2 -translate-y-1/2 font-semibold">10</span>
                  ${faSprite_('time-skip', 'regular', 'h-5 w-5 rotate-y-180')}
                </button>
              </div>
              <div></div>
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
              <div class="dropdown">
                <div tabindex="0" role="button" class="cursor-pointer">Console</div>
                <ul tabindex="0" class="dropdown-content menu space-y-1 text-xs bg-base-100 border w-max rounded-box z-1 p-1 shadow">
                  <li>
                    <button
                      class="px-4 hover:bg-fillweak py-1 ${this.consoleEventsEnable[0] ? 'bg-fillBrand-weak text-blue-700' : ''}"
                      @click=${() => this.toggleConsoleEvent(0)}
                    >
                      Error (${this.consoleTypesCounts.error})
                    </button>
                  </li>
                  <li>
                    <button
                      class="px-4 rounded hover:bg-fillweak py-1 ${this.consoleEventsEnable[1] ? 'bg-fillBrand-weak text-blue-700' : ''}"
                      @click=${() => this.toggleConsoleEvent(1)}
                    >
                      Warn (${this.consoleTypesCounts.warn})
                    </button>
                  </li>
                  <li>
                    <button
                      class="px-4 rounded hover:bg-fillweak py-1 ${this.consoleEventsEnable[2] ? 'bg-fillBrand-weak text-blue-700' : ''}"
                      @click=${() => this.toggleConsoleEvent(2)}
                    >
                      Info (${this.consoleTypesCounts.info})
                    </button>
                  </li>
                </ul>
              </div>
            </div>
            ${this.activityWidth > 0
              ? html`<button class="cursor-pointer hover:bg-fillWeak" @click=${this.closePlayerWindow}>
                  ${faSprite_('circle-xmark', 'regular', 'w-4 h-4')}
                </button>`
              : nothing}
          </div>
          <div
            class="flex flex-col h-full overflow-y-auto w-full overflow-x-hidden c-scroll scroll-smooth"
            style="height:calc(100% - 80px)"
          >
            ${this.consoleEvents.map((e) => this.displayConsoleEvent(e))}
          </div>
          <div class="flex items-center h-10 bg-fillWeak border-t">
            <button
              @click=${() => (this.syncScrolling = !this.syncScrolling)}
              class="text-xs font-semibold h-full px-2 hover:bg-fillWeaker ${this.syncScrolling ? 'text-textBrand' : ''}"
            >
              Sync scrolling
            </button>
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
