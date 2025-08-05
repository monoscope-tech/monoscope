import { html, LitElement, nothing, PropertyValues } from 'lit';
import { customElement, property, query, state } from 'lit/decorators.js';
import rrwebPlayer from 'rrweb-player';
import { EventType, eventWithTime } from '@rrweb/types';
import { faSprite_ } from './monitors/test-editor-utils';
import { ConsoleEvent } from './types/types';
// import 'rrweb-player/dist/style.css';
import { Replayer } from '@rrweb/replay';
@customElement('session-replay')
export class SessionReplay extends LitElement {
  @property({ type: String }) private events: string = '[]';

  @state() private activityWidth = 0;
  @query('#replayerOuterContainer') private replayerOuterContainer;
  @state() private playSpeed = 1;
  @state() private skipInactive = true;
  @state() private consoleEventsEnable = [true, true, true]; // error, warn, info;
  @state() private paused = false;

  @state() private consoleEvents: ConsoleEvent[] = [];

  private startX: number | null = null;
  private currentTime = 0;
  private player;
  private containerWidth = 1024;
  private containerHeight = 500;
  private iframeWidth = 1117;
  private iframeHeight = 927;
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

    this.updateScale = this.updateScale.bind(this);
    this.handleConsoleEvents = this.handleConsoleEvents.bind(this);

    document.addEventListener('mousemove', (e) => {
      if (this.startX !== null) {
        const diff = this.startX - e.clientX;
        const curr = this.activityWidth + diff;
        this.activityWidth = curr;
        this.startX = e.clientX;
      }
    });
  }

  handleConsoleEvents(event: eventWithTime) {
    if (event.type === EventType.Plugin && event.data.plugin === 'rrweb/console@1') {
      this.consoleEvents = [...this.consoleEvents, event as ConsoleEvent];
    }
  }

  updateContainerWidths() {
    const frameContainer = document.querySelector('.player-frame') as HTMLElement;
    frameContainer.style.width = `${this.containerWidth}px`;
    frameContainer.style.height = `${this.containerHeight}px`;
  }
  updateScale = () => {
    this.updateContainerWidths();

    const el = this.player.wrapper;
    const widthScale = this.containerWidth / this.iframeWidth;
    const heightScale = this.containerHeight / this.iframeHeight;
    el.style.transform = `scale(${Math.min(widthScale, heightScale)})` + 'translate(-50%, -50%)';
  };

  protected updated(changedProperties: PropertyValues): void {
    if (this.player) {
      const mContainer = Number(getComputedStyle(this.replayerOuterContainer).width.replace('px', ''));

      if (changedProperties.has('activityWidth')) {
        this.containerWidth = mContainer - this.activityWidth;
        this.updateScale();
      }
      if (changedProperties.has('skipInactive')) {
        this.player.toggleSkipInactive();
      }
      if (changedProperties.has('playSpeed')) {
        this.player.setSpeed(this.playSpeed);
      }
      if (changedProperties.has('paused')) {
        if (this.paused) {
          this.currentTime = this.player.getCurrentTime();
        }
        this.paused ? this.player.pause() : this.player.play(this.currentTime);
      }
    }
  }

  displayConsoleEvent = (event: ConsoleEvent) => {
    const payload = event.data.payload;
    switch (payload.level) {
      case 'error':
        return html`
          <div class="text-sm flex flex-col min-w-0 event-container">
            <div class="flex items-center w-full">
              <span class="text-xs font-medium text-center text-textWeak min-w-11">6:25</span>
              ${faSprite_('console', 'regular', 'w-2.5 h-2.5 mx-1 font-semibold')}
              <span class="w-full min-w-0 truncate px-2 pb-1 overflow-ellipsis hover:bg-fillError-weak bg-red-100"
                >${payload.payload.join('').substring(0, 100)}</span
              >
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
            <div class="flex-col bg-red-100 p-2 w-full min-w-0 hidden event-detail">
              <div class="bg-bgBase rounded-xl p-3 whitespace-pre-wraps text-textError font-medium">${payload.payload.join('\n')}</div>
              <span class="mt-4 mb-1 font-semibold">Stack trace</span>
              <div class="bg-bgBase rounded-xl p-3 whitespace-pre-wraps font-medium">${payload.trace.join('\n')}</div>
            </div>
          </div>
        `;

      default:
        return html`<p>..</p>`;
    }
  };

  toggleConsoleEvent(indx: number) {
    this.consoleEventsEnable[indx] = !this.consoleEventsEnable[indx];
    this.consoleEventsEnable = [...this.consoleEventsEnable];
  }

  protected firstUpdated(_changedProperties: PropertyValues): void {
    const target = document.querySelector('#playerWrapper') as HTMLElement;
    const mContainer = Number(getComputedStyle(this.replayerOuterContainer).width.replace('px', ''));
    this.containerWidth = mContainer - this.activityWidth;
    this.player = new Replayer(JSON.parse(localStorage.getItem('vvv') || '[]'), {
      root: target, // customizable root element
      // props: {
      // events:
      plugins: [{ handler: this.handleConsoleEvents }],
      skipInactive: this.skipInactive,
      // },
    });
    this.updateScale();
    this.player.play();
  }

  render() {
    return html`<div class="w-full h-screen flex overflow-x-hidden" id="replayerOuterContainer">
      <div class="w-full h-full flex flex-col justify-start shrink-1">
        <div class="bg-fillWeaker w-full px-2 py-1 flex items-center border-b gap-4 justify-between">
          <div class="flex items-center gap-4 shrink-1">
            <h3 class="font-medium h-full truncate overflow-ellipsis min-w-0">Session recording</h3>
          </div>

          <div class="flex items-center gap-4 text-xs font-semibold">
            <div class="dropdown">
              <div tabindex="0" role="button" class="cursor-pointer flex items-center gap-1 ${this.playSpeed != 1 ? 'text-blue-500' : ''}">
                ${faSprite_('gauge', 'regular', 'w-2.5 h-2.5')} Speed ${this.playSpeed}x
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
              ${faSprite_('gauge', 'regular', 'w-2.5 h-2.5')}
              <span>Skip inactive</span>
            </button>
            <button
              @click=${() => {
                this.activityWidth = this.activityWidth <= 0 ? 500 : 0;
              }}
              class="cursor-pointer flex items-center gap-1 ${this.activityWidth > 0 ? 'text-blue-500' : ''}"
            >
              ${faSprite_('side-chevron-left-in-box', 'regular', 'w-2.5 h-2.5')} Activity
            </button>
          </div>
        </div>
        <!-- End nav controls -->
        <div class="relative" style="height:${this.containerHeight + 80}px">
          <div class="border-b relative bg-black">
            <div
              class="player-frame border-y"
              id="playerWrapper"
              style="height:${this.containerHeight}px; width: ${this.containerWidth}px"
            ></div>
            <div class="absolute inset-0 flex bg-fillInverse-weak items-center justify-center ${this.paused ? '' : 'hidden'}">
              <button>${faSprite_('play', 'regular', 'w-20 h-20')}</button>
            </div>
          </div>
          <div class="w-full border-b flex items-center justify-center">
            <button>prv</button>
            <button class="w-14 h-14 flex justify-center items-center" @click=${() => (this.paused = !this.paused)}>
              ${this.paused ? faSprite_('play', 'regular', 'h-12 w-12') : faSprite_('play', 'regular', 'h-12 w-12')}
            </button>
            <button>next</button>
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
          <div class="bg-fillWeaker w-full px-4 py-1 flex items-center border-b gap-4 justify-between">
            <div class="flex items-center gap-4 text-xs font-semibold">
              <div class="dropdown">
                <div tabindex="0" role="button" class="cursor-pointer">Console</div>
                <ul tabindex="0" class="dropdown-content menu space-y-1 text-xs bg-base-100 border rounded-box z-1 p-1 shadow">
                  <li>
                    <button
                      class="px-4 hover:bg-fillweak py-1 ${this.consoleEventsEnable[0] ? 'bg-blue-100 text-blue-700' : ''}"
                      @click=${() => this.toggleConsoleEvent(0)}
                    >
                      Error
                    </button>
                  </li>
                  <li>
                    <button
                      class="px-4 rounded hover:bg-fillweak py-1 ${this.consoleEventsEnable[1] ? 'bg-blue-100 text-blue-700' : ''}"
                      @click=${() => this.toggleConsoleEvent(1)}
                    >
                      Warn
                    </button>
                  </li>
                  <li>
                    <button
                      class="px-4 rounded hover:bg-fillweak py-1 ${this.consoleEventsEnable[2] ? 'bg-blue-100 text-blue-700' : ''}"
                      @click=${() => this.toggleConsoleEvent(2)}
                    >
                      Info
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
            <h3 class="font-medium">.</h3>
          </div>
          <div class="flex flex-col h-full overflow-y-auto w-full overflow-x-hidden c-scroll">
            ${this.consoleEvents.map((e) => this.displayConsoleEvent(e))}
          </div>
        </div>
      </div>
    </div>`;
  }
  createRenderRoot() {
    return this;
  }
}
