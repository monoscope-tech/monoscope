import { html, LitElement, PropertyValues } from 'lit';
import { customElement, property, query, state } from 'lit/decorators.js';
import rrwebPlayer from 'rrweb-player';
import { EventType, eventWithTime } from '@rrweb/types';
import { faSprite_ } from './monitors/test-editor-utils';
import { ConsoleEvent } from './types/types';

@customElement('session-replay')
export class SessionReplay extends LitElement {
  @property({ type: String }) private events: string = '[]';

  @state() private activityWidth = 0;
  @query('#replayerOuterContainer') private replayerOuterContainer;

  @state() private consoleEvents: ConsoleEvent[] = [];

  private startX: number | null = null;
  private player;
  private containerWidth = 1024;
  private containerHeight = 600;
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
    const playerContainer = document.querySelector('.rr-player') as HTMLElement;
    const frameContainer = document.querySelector('.rr-player__frame') as HTMLElement;
    playerContainer.style.width = `${this.containerWidth}px`;
    frameContainer.style.width = `${this.containerWidth}px`;
  }
  updateScale = () => {
    this.updateContainerWidths();
    const player = this.player.getReplayer();

    const el = player.wrapper;
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
              ${faSprite_('copy', 'regular', 'w-2.5 h-2.5 mx-1 font-semibold')}
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

  protected firstUpdated(_changedProperties: PropertyValues): void {
    const target = document.querySelector('#playerWrapper') as HTMLElement;
    const mContainer = Number(getComputedStyle(this.replayerOuterContainer).width.replace('px', ''));
    const width = (this.containerWidth = mContainer - this.activityWidth);
    const height = this.containerHeight / (4 / 3);
    this.player = new rrwebPlayer({
      target, // customizable root element
      props: {
        events: JSON.parse(this.events || '[]'),
        height,
        width,
        plugins: [{ handler: this.handleConsoleEvents }],
      },
    });
  }

  render() {
    return html`<div class="w-full h-full flex overflow-x-hidden" id="replayerOuterContainer">
      <div class="w-full h-full shrink-1">
        <div class="bg-fillWeaker w-full px-2 py-1 flex items-center border-b gap-4 justify-between">
          <div class="flex items-center gap-4 shrink-1">
            <h3 class="font-medium h-full truncate overflow-ellipsis min-w-0">Session recording</h3>
          </div>

          <div class="flex items-center gap-4 text-xs font-semibold">
            <div class="dropdown">
              <div tabindex="0" role="button" class="cursor-pointer flex items-center gap-1">
                ${faSprite_('gauge', 'regular', 'w-2.5 h-2.5')} Speed 1x
              </div>
              <ul tabindex="0" class="dropdown-content menu bg-base-100 rounded-box z-1 w-52 p-2 shadow-sm">
                <li><a>Item 1</a></li>
                <li><a>Item 2</a></li>
              </ul>
            </div>
            <button class="flex items-center cursor-pointer gap-1">
              ${faSprite_('gauge', 'regular', 'w-2.5 h-2.5')}
              <span>Skip inactivity</span>
            </button>
            <button
              @click=${() => {
                this.activityWidth = this.activityWidth <= 0 ? 500 : this.activityWidth;
              }}
              class="cursor-pointer flex items-center gap-1"
            >
              ${faSprite_('side-chevron-left-in-box', 'regular', 'w-2.5 h-2.5')} Activity
            </button>
          </div>
        </div>
        <!-- End nav controls -->
        <div id="playerWrapper" class="w-full bg-black border-b-2 border-amber-200"></div>
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
                <ul tabindex="0" class="dropdown-content menu bg-base-100 rounded-box z-1 w-52 p-2 shadow-sm">
                  <li><a>Item 1</a></li>
                  <li><a>Item 2</a></li>
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
