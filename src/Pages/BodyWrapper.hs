module Pages.BodyWrapper (bodyWrapper, BWConfig (..), PageCtx (..), onboardingChecklist_, settingsContentTarget, navTabAttrs) where

import Data.CaseInsensitive qualified as CI
import Data.Default (Default)
import Data.Text qualified as T
import Data.Tuple.Extra (fst3)
import Data.Vector qualified as V
import Lucid
import Lucid.Aria qualified as Aria
import Lucid.Htmx (hxGet_, hxIndicator_, hxPost_, hxPushUrl_, hxSelect_, hxSwap_, hxTarget_, hxTrigger_, hxVals_)
import Lucid.Hyperscript (__)
import Models.Projects.Projects qualified as Projects
import NeatInterpolation (text)
import Pages.Components qualified as Components
import Pkg.DeriveUtils (hashAssetFile)
import PyF
import Relude
import System.Config (EnvConfig (..))
import Utils (FreeTierStatus (..), LoadingSize (..), LoadingType (..), faSprite_, freeTierUsageBanner, loadingIndicatorWith_, loadingIndicator_, navTabAttrs)


menu :: Projects.ProjectId -> [(Text, Text, Text)]
menu pid =
  [ ("Dashboards", "/p/" <> pid.toText <> "/dashboards", "dashboard")
  , ("Explorer", "/p/" <> pid.toText <> "/log_explorer", "explore")
  , ("API Catalog", "/p/" <> pid.toText <> "/api_catalog", "swap")
  , ("Issues", "/p/" <> pid.toText <> "/issues", "bug")
  , ("Monitors", "/p/" <> pid.toText <> "/monitors", "list-check")
  , ("Reports", "/p/" <> pid.toText <> "/reports", "chart-simple")
  ]


-- | Onboarding checklist widget for the sidenav
onboardingChecklist_ :: Projects.Project -> Html ()
onboardingChecklist_ project = do
  let steps = project.onboardingStepsCompleted
      pid = project.id.toText
      hasEvents = V.elem "Integration" steps || V.elem "has_events" steps
      exploredLogs = V.elem "explored_logs" steps
      createdMonitor = V.elem "created_monitor" steps
      setupNotifs = V.elem "NotifChannel" steps
      items =
        [ (hasEvents, "Send first event", "/p/" <> pid <> "/onboarding?step=Integration", "paper-plane")
        , (exploredLogs, "Explore logs", "/p/" <> pid <> "/log_explorer", "magnifying-glass")
        , (createdMonitor, "Create a monitor", "/p/" <> pid <> "/monitors", "bell")
        , (setupNotifs, "Set up notifications", "/p/" <> pid <> "/settings/integrations", "envelope")
        ]
          :: [(Bool, Text, Text, Text)]
      doneCount = length (filter (\(d, _, _, _) -> d) items)
      totalCount = length items
      allDone = doneCount == totalCount
      dismissed = V.elem "checklist_dismissed" steps
  unless (allDone || dismissed)
    $ div_ [id_ "onboarding-checklist", class_ "mt-5 pt-3 border-t border-strokeWeak"] do
      -- Collapsed state: rocket icon
      div_ [class_ "flex justify-center group-has-[#sidenav-toggle:checked]/pg:hidden"] do
        a_ [href_ $ "/p/" <> pid <> "/onboarding", class_ "relative tap-target", term "data-tippy-placement" "right", term "data-tippy-content" $ "Getting Started (" <> show doneCount <> "/" <> show totalCount <> ")"] do
          faSprite_ "rocket" "regular" "w-4 h-4 text-textWeak"
      -- Expanded state: full checklist
      div_ [class_ "hidden group-has-[#sidenav-toggle:checked]/pg:block bg-fillWeaker rounded-lg p-2.5"] do
        div_ [class_ "flex items-center justify-between mb-1.5 pl-1.5"] do
          div_ [class_ "flex items-center gap-2"] do
            faSprite_ "rocket" "regular" "w-2.5 h-2.5 text-textWeak"
            span_ [class_ "text-xs font-medium text-textStrong"] "Getting Started"
          div_ [class_ "flex items-center gap-3"] do
            span_ [class_ "text-xs text-textWeak tabular-nums"] $ toHtml @Text $ show doneCount <> "/" <> show totalCount
            button_
              [ class_ "text-textWeak opacity-50 hover:opacity-100 hover:text-textStrong tap-target cursor-pointer"
              , Aria.label_ "Dismiss getting started checklist"
              , hxPost_ $ "/p/" <> pid <> "/onboarding/dismiss-checklist"
              , hxTarget_ "#onboarding-checklist"
              , hxSwap_ "delete"
              ]
              $ faSprite_ "xmark" "regular" "w-2.5 h-2.5"
        div_ [class_ "h-0.5 w-full bg-strokeWeak rounded-full overflow-hidden mb-2"] do
          let pct = show (doneCount * 100 `div` totalCount :: Int)
          div_ [class_ "h-full bg-strokeBrand-strong rounded-full transition-all", style_ $ "width:" <> toText pct <> "%"] ""
        let sorted = sortOn (Down . (\(d, _, _, _) -> d)) items
        div_ [class_ "flex flex-col gap-0.5"] do
          forM_ sorted \(done, label, link, icon) ->
            a_
              [ href_ link
              , class_ $ "flex items-center gap-2 px-2 py-1 rounded-md text-xs transition-colors " <> if done then "text-textWeak opacity-60" else "text-textStrong font-medium hover:bg-fillWeak"
              ]
              do
                if done
                  then faSprite_ "circle-check" "solid" "w-3.5 h-3.5 text-textSuccess shrink-0"
                  else faSprite_ icon "regular" "w-3.5 h-3.5 shrink-0"
                span_ [class_ "truncate"] $ toHtml label


type role PageCtx representational


data PageCtx a = PageCtx
  { conf :: BWConfig
  , content :: a
  }
  deriving stock (Generic, Show)


instance ToHtml a => ToHtml (PageCtx a) where
  {-# INLINE toHtml #-}
  toHtml (PageCtx bwcfg child) = toHtmlRaw $ bodyWrapper bwcfg (toHtml child)
  {-# INLINE toHtmlRaw #-}
  toHtmlRaw (PageCtx bwcfg child) = toHtmlRaw $ bodyWrapper bwcfg (toHtmlRaw child)


-- TODO: Rename to pageCtx
data BWConfig = BWConfig
  { sessM :: Maybe Projects.Session
  , currProject :: Maybe Projects.Project
  , prePageTitle :: Maybe Text
  , pageTitle :: Text
  , pageTitleSuffix :: Maybe Text -- Additional breadcrumb after pageTitle (e.g., tab name)
  , pageTitleModalId :: Maybe Text -- Modal ID for renaming page title
  , pageTitleSuffixModalId :: Maybe Text -- Modal ID for renaming suffix (e.g., tab)
  , menuItem :: Maybe Text -- Use PageTitle if menuItem is not set
  , navTabs :: Maybe (Html ())
  , pageActions :: Maybe (Html ())
  , docsLink :: Maybe Text
  , isSettingsPage :: Bool
  , freeTierStatus :: FreeTierStatus
  , hideNavbar :: Bool -- When True, hides the entire navbar
  , headContent :: Maybe (Html ()) -- Optional HTML content to include in the head
  , config :: EnvConfig -- Environment configuration for telemetry
  }
  deriving stock (Generic, Show)
  deriving anyclass (Default)


bodyWrapper :: BWConfig -> Html () -> Html ()
bodyWrapper bcfg child = do
  doctypehtml_ do
    head_
      do
        title_ $ toHtml bcfg.pageTitle
        meta_ [charset_ "UTF-8"]
        meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
        meta_ [httpEquiv_ "X-UA-Compatible", content_ "ie=edge"]
        meta_ [name_ "htmx-config", content_ [text|{"selfRequestsOnly":false}|]]
        -- favicon items
        link_ [rel_ "apple-touch-icon", sizes_ "180x180", href_ "/public/apple-touch-icon.png"]
        link_ [rel_ "icon", type_ "image/png", sizes_ "32x32", href_ "/public/favicon-32x32.png"]
        link_ [rel_ "icon", type_ "image/png", sizes_ "16x16", href_ "/public/favicon-16x16.png"]
        link_ [rel_ "manifest", href_ "/public/site.webmanifest"]
        link_ [rel_ "mask-icon", href_ "/public/safari-pinned-tab.svg", term "color" "#5bbad5"]
        meta_ [name_ "msapplication-TileColor", content_ "#da532c"]
        meta_ [name_ "theme-color", content_ "#ffffff"]

        -- Resource hints for faster loading
        link_ [rel_ "preconnect", href_ "https://www.gravatar.com"]
        link_ [rel_ "preconnect", href_ "https://ui-avatars.com"]
        link_ [rel_ "dns-prefetch", href_ "https://cdn.jsdelivr.net"]
        link_ [rel_ "dns-prefetch", href_ "https://unpkg.com"]

        -- Preload critical CSS
        link_ [rel_ "preload", href_ $(hashAssetFile "/public/assets/css/tailwind.min.css"), term "as" "style"]

        -- View Transitions API (Chrome 111+, graceful fallback for others)
        meta_ [name_ "view-transition", content_ "same-origin"]
        style_
          """
          @supports (view-transition-name: root) {
            ::view-transition-old(root) { animation: vt-fade-out 150ms ease-out; }
            ::view-transition-new(root) { animation: vt-fade-in 150ms ease-in; }
          }
          @keyframes vt-fade-out { from { opacity: 1; } to { opacity: 0; } }
          @keyframes vt-fade-in { from { opacity: 0; } to { opacity: 1; } }
          """

        link_ [rel_ "stylesheet", type_ "text/css", href_ $(hashAssetFile "/public/assets/css/thirdparty/notyf3.min.css")]
        link_ [rel_ "stylesheet", href_ $(hashAssetFile "/public/assets/css/thirdparty/tagify.min.css"), type_ "text/css"]
        link_ [rel_ "stylesheet", href_ $(hashAssetFile "/public/assets/deps/gridstack/gridstack.min.css")]
        link_ [rel_ "stylesheet", href_ $(hashAssetFile "/public/assets/css/thirdparty/rrweb.css")]

        link_ [rel_ "stylesheet", type_ "text/css", href_ $(hashAssetFile "/public/assets/css/tailwind.min.css")]
        link_ [rel_ "stylesheet", type_ "text/css", href_ $(hashAssetFile "/public/assets/web-components/dist/css/index.css")]

        -- Include optional head content from the page
        whenJust bcfg.headContent id

        script_ [src_ $(hashAssetFile "/public/assets/deps/tagify/tagify.min.js")] ("" :: Text)
        script_ [src_ $(hashAssetFile "/public/assets/deps/echarts/echarts.min.js")] ("" :: Text)
        script_ [src_ $(hashAssetFile "/public/assets/roma-echarts.js")] ("" :: Text)
        script_ [src_ $(hashAssetFile "/public/assets/js/thirdparty/notyf3.min.js"), defer_ "true"] ("" :: Text)
        script_ [src_ "https://cdn.jsdelivr.net/npm/htmx.org@2.0.8/dist/htmx.min.js"] ("" :: Text)
        script_ [src_ $(hashAssetFile "/public/assets/deps/htmx/multi-swap.js"), defer_ "true"] ("" :: Text)
        script_ [src_ $(hashAssetFile "/public/assets/deps/htmx/preload.js"), defer_ "true"] ("" :: Text)
        script_ [src_ $(hashAssetFile "/public/assets/deps/htmx/json-enc-2.js"), defer_ "true"] ("" :: Text)
        script_ [src_ $(hashAssetFile "/public/assets/deps/htmx/response-targets.js"), defer_ "true"] ("" :: Text)
        script_ [src_ $(hashAssetFile "/public/assets/deps/htmx/idiomorph-ext.min.js"), defer_ "true"] ("" :: Text)
        script_ [src_ $(hashAssetFile "/public/assets/deps/lit/lit-html.js"), type_ "module", defer_ "true"] ("" :: Text)
        script_ [src_ $(hashAssetFile "/public/assets/deps/gridstack/gridstack-all.js")] ("" :: Text)

        script_ [src_ $(hashAssetFile "/public/assets/deps/easepick/bundle.min.js")] ("" :: Text)

        script_ [src_ $(hashAssetFile "/public/assets/js/thirdparty/_hyperscript_web0_9_5.min.js"), defer_ "true"] ("" :: Text)
        script_ [src_ $(hashAssetFile "/public/assets/js/thirdparty/_hyperscript_template.js"), defer_ "true"] ("" :: Text)
        script_ [src_ $(hashAssetFile "/public/assets/js/thirdparty/luxon.min.js"), defer_ "true"] ("" :: Text)
        script_ [src_ $(hashAssetFile "/public/assets/js/thirdparty/popper2_11_4.min.js"), defer_ "true"] ("" :: Text)
        script_ [src_ $(hashAssetFile "/public/assets/js/thirdparty/tippy6_3_7.umd.min.js"), defer_ "true"] ("" :: Text)
        -- script_ [src_ $(hashAssetFile "/public/assets/js/thirdparty/instantpage5_1_0.js"), type_ "module", defer_ "true"] ("" :: Text)
        script_ [src_ $(hashAssetFile "/public/assets/js/main.js")] ("" :: Text)

        when bcfg.config.enableBrowserMonitoring $ script_ [src_ "https://unpkg.com/@monoscopetech/browser@latest/dist/monoscope.min.js"] ("" :: Text)

        -- Flag for widget initialization - set to true after web-components loads
        script_ "window.widgetDepsReady = false;"
        script_ [type_ "module", src_ $(hashAssetFile "/public/assets/web-components/dist/js/index.js")] ("" :: Text)

        script_
          [text|
      !function(e,t,n,s,u,a){e.twq||(s=e.twq=function(){s.exe?s.exe.apply(s,arguments):s.queue.push(arguments);},s.version='1.1',s.queue=[],u=t.createElement(n),u.async=!0,u.src='https://static.ads-twitter.com/uwt.js',
      a=t.getElementsByTagName(n)[0],a.parentNode.insertBefore(u,a))}(window,document,'script');
      twq('config','om5gt');
      |]

        let swURI = $(hashAssetFile "/public/sw.js")
        script_
          [text|
        if("serviceWorker" in navigator) {
            window.addEventListener("load", () => {
              navigator.serviceWorker.register("${swURI}").then(swReg => {}).catch(err => {
                  console.error('Service Worker Error', err);
              });
          });
        }
          |]
        script_
          [raw|


        function navigatable(me, target, container, activeClass, tabPrefix)  {
            const tabeName = tabPrefix ? tabPrefix + "-tab" : "a-tab";
            const contentName = tabPrefix ? tabPrefix + "-tab-content" : "a-tab-content";
            const nav = document.querySelector(container);
            const tabs = nav.querySelectorAll("." + tabeName);
            const contents = nav.querySelectorAll("." + contentName);
            const targetElement = document.querySelector(target);
            
            // Batch DOM updates using requestAnimationFrame
            requestAnimationFrame(() => {
              tabs.forEach(tab => {
                tab.classList.remove(activeClass);
              });
              me.classList.add(activeClass);
              contents.forEach(content => content.classList.add("hidden"));
              targetElement.classList.remove("hidden");
              targetElement.dispatchEvent(new CustomEvent("tab-visible", { bubbles: true }));
            });
        }

        function setCookie(cname, cvalue, exdays = 365) {
            const d = new Date();
            d.setTime(d.getTime() + (exdays * 24 * 60 * 60 * 1000));
            const expires = "expires=" + d.toUTCString();
            document.cookie = `${cname}=${cvalue};${expires};path=/`;
        }

        function getCookie(cname) {
            const name = `${cname}=`;
            const decodedCookie = decodeURIComponent(document.cookie);
            const ca = decodedCookie.split(';');
            for (let i = 0; i < ca.length; i++) {
                let c = ca[i].trim();
                if (c.startsWith(name)) return c.substring(name.length);
            }
            return "";
        }

        var currentISOTimeStringVar = ((new Date()).toISOString().split(".")[0])+"+00:00";
        document.addEventListener('DOMContentLoaded', function(){
          // htmx.config.useTemplateFragments = true
          // Tooltip warmth tracking - skip delay when moving between tooltips
          let tooltipWarmTimeout;
          let isTooltipWarm = false;

          // Event delegation for tooltips - single listener, no querySelectorAll per afterSettle
          document.body.addEventListener('mouseover', function(e) {
            const element = e.target.closest('[data-tippy-content]');
            if (!element || element._tippy) return;

            const instance = tippy(element, {
              delay: [isTooltipWarm ? 0 : 100, 0],
              duration: 0,
              updateDuration: 0,
              animateFill: false,
              moveTransition: '',
              animation: false,
              touch: false,
              followCursor: false,
              flipOnUpdate: false,
              lazy: true,
              onShow() {
                isTooltipWarm = true;
                clearTimeout(tooltipWarmTimeout);
              },
              onHide() {
                tooltipWarmTimeout = setTimeout(() => { isTooltipWarm = false; }, 300);
              },
              popperOptions: {
                strategy: 'absolute',
                modifiers: [{
                  name: 'computeStyles',
                  options: { gpuAcceleration: true, adaptive: false },
                }],
              },
            });
            instance.show();
          });

          // Clear tooltip warmth timeout on page unload and HTMX navigation to prevent memory leak
          window.addEventListener('beforeunload', () => clearTimeout(tooltipWarmTimeout));
          document.body.addEventListener('htmx:beforeSwap', () => clearTimeout(tooltipWarmTimeout));

          // Animate stat values on HTMX content swap for delightful updates
          document.body.addEventListener('htmx:afterSwap', (e) => {
            e.target.querySelectorAll('.stat-value[data-value]').forEach(el => {
              const newVal = parseFloat(el.dataset.value);
              if (!isNaN(newVal) && typeof window.animateStatValue === 'function') {
                window.animateStatValue(el, newVal, 400);
              }
            });
          });

          // Add aria-busy during HTMX requests for screen reader feedback
          document.body.addEventListener('htmx:beforeRequest', (e) => {
            e.target.setAttribute('aria-busy', 'true');
          });
          document.body.addEventListener('htmx:afterRequest', (e) => {
            e.target.removeAttribute('aria-busy');
          });

          // Progress bar for HTMX requests
          const progressBar = document.getElementById('htmx-progress');
          if (progressBar) {
            document.body.addEventListener('htmx:beforeRequest', () => {
              progressBar.classList.remove('htmx-settling');
              progressBar.classList.add('htmx-request');
            });
            document.body.addEventListener('htmx:afterRequest', () => {
              progressBar.classList.remove('htmx-request');
              progressBar.classList.add('htmx-settling');
            });
          }

          // Cmd+Enter / Ctrl+Enter form submission for textareas
          document.addEventListener('keydown', function(e) {
            if ((e.metaKey || e.ctrlKey) && e.key === 'Enter' && e.target.tagName === 'TEXTAREA') {
              const form = e.target.closest('form');
              if (form) {
                e.preventDefault();
                form.requestSubmit();
              }
            }
          });

          var notyf = new Notyf({
              duration: 5000,
              position: {x: 'right', y: 'top'},
          });
          const toastAnnouncer = document.getElementById('toast-announcer');
          document.body.addEventListener("successToast", (e)=> {
            e.detail.value.map(v => {
              notyf.success(v);
              if (toastAnnouncer) toastAnnouncer.textContent = v;
            });
          });
          document.body.addEventListener("errorToast", (e)=> {
            e.detail.value.map(v => {
              notyf.error(v);
              if (toastAnnouncer) toastAnnouncer.textContent = 'Error: ' + v;
            });
          });
        });
        
    function filterByField(event, operation) {
        const pathsToRemap = [
          ["request_headers", "attributes.http.request.header"],
          ["response_headers", "attributes.http.response.header"],
          ["response_body", "body.response_body"],
          ["request_body", "body.request_body"],
          ["method", "attributes.http.request.method"],
          ["query_params", "attributes.http.request.query_params"],
          ["path_params", "attributes.http.request.path_params"],
          ["host", "attributes.net.host.name"],
          ["urlPath", "attributes.http.route"],
          ["raw_url", "attributes.http.target"],
          ["status_code", "attributes.http.response.status_code"],
        ]
        let { fieldPath: path, fieldValue: value } = event.target.closest('[data-field-path]').dataset;

        pathsToRemap.forEach(([from, to]) => {
          if (path.startsWith(from)) {
            path = path.replace(from, to)
          }
        })

        const operator = operation === 'Eq' ? '==' : operation === 'NotEq' ? '!=' : '==';
        document.getElementById("filterElement").handleAddQuery(path + ' ' + operator + ' ' + value);
    }

    function viewFieldPatterns(fieldPath) {
        const url = new URL(window.location.href);
        url.searchParams.set('viz_type', 'patterns');
        url.searchParams.set('pattern_target', fieldPath);
        url.searchParams.delete('pattern_skip');
        window.location.href = url.toString();
    }

    var toggleColumnToSummary = (e)=>{
      const cols = (params().cols||"").split(",").filter(x=>x!="");
      const subject = (e.target.closest('[data-field-path]')?.dataset.fieldPath || e.target.closest('[data-field]').dataset.field);
      const finalCols =  subject ? 
        [...new Set(cols.includes(subject) ? 
          cols.filter(x=>x!=subject) : 
          [...cols, subject])].join(",") : 
        cols.join(",");
      return finalCols;
    }


    var removeNamedColumnToSummary = (namedCol) => {
      const cols = (params().cols ?? '').split(',').filter((x) => x != '')
      return [...new Set(cols.filter((x) => namedCol.toLowerCase() != x.replaceAll('.', '•').replaceAll('[', '❲').replaceAll(']', '❳').toLowerCase()))].join(',')
    }

      |]
        script_
          [type_ "text/hyperscript"]
          [text|
          behavior LogItemMenuable
            on click
              log "clicked" then
              if I match <.with-context-menu/> then
                remove <.log-item-context-menu /> then remove .with-context-menu from <.with-context-menu />
              else
                remove <.log-item-context-menu /> then remove .with-context-menu from <.with-context-menu /> then
                get #log-item-context-menu-tmpl.innerHTML then put it after me then add .with-context-menu to me then
                _hyperscript.processNode(.log-item-context-menu) then htmx.process(next <.log-item-context-menu/>)
              end
            end
          end
          behavior Copy(content)
               on click if 'clipboard' in window.navigator then
                    call navigator.clipboard.writeText(content's innerText)
                    add .copy-success to me then
                    wait 1500ms then remove .copy-success from me then
                    send successToast(value:['Value copied to the Clipboard']) to <body/>
                    halt
              end
            end
    |]

    body_ [class_ "h-full w-full bg-bgBase text-textStrong group/pg", term "data-theme" (maybe "dark" (.theme) bcfg.sessM), term "hx-ext" "multi-swap,preload,response-targets,morph", term "preload" "mouseover"] do
      -- Skip to main content link for keyboard users (accessibility)
      a_ [class_ "sr-only focus:not-sr-only focus:absolute focus:top-4 focus:left-4 focus:z-[100000] focus:bg-bgRaised focus:px-4 focus:py-2 focus:rounded-lg focus:text-textBrand focus:shadow-lg focus:ring-2 focus:ring-strokeFocus", href_ "#main-content"] "Skip to main content"
      -- ARIA live region for toast announcements (screen reader accessibility)
      div_ [id_ "toast-announcer", Aria.live_ "polite", Aria.atomic_ "true", class_ "sr-only"] ""
      -- HTMX progress bar for long operations
      div_ [id_ "htmx-progress", class_ "htmx-progress"] ""
      case bcfg.sessM of
        Nothing -> do
          section_ [class_ "flex flex-col grow  h-screen overflow-y-hidden"]
            $ section_
              [class_ "flex-1 overflow-y-auto"]
              child
        Just sess ->
          let currUser = sess.persistentSession.user.getUser
              sideNav' = bcfg.currProject & maybe "" \project -> sideNav sess project (fromMaybe bcfg.pageTitle bcfg.prePageTitle) bcfg.menuItem
           in do
                -- Command palette global shortcut (Cmd+K / Ctrl+K)
                whenJust bcfg.currProject \p ->
                  span_
                    [ data_ "palette-url" ("/p/" <> p.id.toText <> "/command-palette")
                    , [__|on keydown[key=='k' and (metaKey or ctrlKey)] from window
                    halt the event
                    if <.cmd-palette-backdrop/> exists
                      remove <.cmd-palette-backdrop/>
                    else
                      set :url to my.dataset.paletteUrl
                      fetch `${:url}` then put the result at start of <body/>
                    end
                  end|]
                    ]
                    ""
                -- Mobile nav toggle (CSS-only sidebar control, only rendered when sidebar exists)
                input_ [type_ "checkbox", class_ "hidden", id_ "mobile-nav-toggle", [__|on load if window.innerWidth < 768 then set #sidenav-toggle.checked to true|]]
                section_ [class_ "flex flex-row grow-0 h-screen overflow-hidden"] do
                  sideNav'
                  section_ [class_ "h-full overflow-y-hidden grow flex flex-col"] do
                    when
                      (currUser.email == "hello@monoscope.tech")
                      loginBanner
                    if bcfg.isSettingsPage || bcfg.hideNavbar
                      then whenJust bcfg.currProject \p -> paletteTriggerFloating p
                      else navbar bcfg.currProject (maybe [] (\p -> menu p.id) bcfg.currProject) currUser bcfg.prePageTitle bcfg.pageTitle bcfg.pageTitleSuffix bcfg.pageTitleModalId bcfg.pageTitleSuffixModalId bcfg.docsLink bcfg.navTabs bcfg.pageActions
                    section_ [id_ "main-content", class_ "overflow-y-auto h-full grow"] do
                      whenJust bcfg.currProject (\p -> freeTierUsageBanner p.id.toText bcfg.freeTierStatus)
                      if bcfg.isSettingsPage
                        then maybe child (\p -> settingsWrapper p.id bcfg.pageTitle child) bcfg.currProject
                        else child
                    div_ [class_ "h-0 shrink"] do
                      Components.drawer_ "global-data-drawer" Nothing Nothing ""
                      template_ [id_ "loader-tmp"] $ loadingIndicator_ LdMD LdDots
                      -- Modal for copying widgets to other dashboards
                      Components.modal_ "dashboards-modal" "" do
                        -- Hidden fields to store widget and dashboard IDs
                        input_ [type_ "hidden", id_ "dashboards-modal-widget-id", name_ "widget_id"]
                        input_ [type_ "hidden", id_ "dashboards-modal-source-dashboard-id", name_ "source_dashboard_id"]

                        div_
                          [ id_ "dashboards-modal-content"
                          , class_ "dashboards-list space-y-3 max-h-160 overflow-y-auto"
                          , hxGet_ ("/p/" <> maybe "" (.id.toText) bcfg.currProject <> "/dashboards?embedded=true")
                          , hxTrigger_ "loadDashboards"
                          , hxSelect_ "#itemsListPage"
                          , hxSwap_ "innerHTML"
                          , hxVals_ "js:{copy_widget_id: document.getElementById('dashboards-modal-widget-id').value, source_dashboard_id: document.getElementById('dashboards-modal-source-dashboard-id').value}"
                          ]
                          do
                            div_ [class_ "skeleton h-16 w-full"] ""
                            div_ [class_ "skeleton h-16 w-full"] ""
                            div_ [class_ "skeleton h-16 w-full"] ""

      -- Mobile nav backdrop (at body level, after section, so it paints on top)
      label_ [term "for" "mobile-nav-toggle", class_ "fixed inset-0 bg-black/50 backdrop-blur-xs z-40 hidden group-has-[#mobile-nav-toggle:checked]/pg:max-md:block cursor-default", Aria.label_ "Close menu"] ""
      externalHeadScripts_ bcfg.config
      alerts_
      script_ [async_ "true", src_ "https://www.googletagmanager.com/gtag/js?id=AW-11285541899"] ("" :: Text)
      script_
        [text|
          window.dataLayer = window.dataLayer || [];
          function gtag(){dataLayer.push(arguments);}
          gtag('js', new Date());
          gtag('config', 'AW-11285541899');

          function gtag_report_conversion(url) {
            var callback = function () {
              if (typeof(url) != 'undefined') {window.location = url;}
            };
            gtag('event', 'conversion', {
                'send_to': 'AW-11285541899/IUBqCKOA-8sYEIvoroUq',
                'event_callback': callback
            });
            return false;
          }

          
          // Dark mode toggle function - disables transitions during theme switch
          function toggleDarkMode() {
            const currentTheme = document.body.getAttribute('data-theme');
            const newTheme = currentTheme === 'dark' ? 'light' : 'dark';

            // Disable transitions during theme switch to prevent flash
            document.documentElement.classList.add('no-transition');
            document.body.setAttribute('data-theme', newTheme);
            setCookie('theme', newTheme, 365);

            // Update all toggle states
            const toggle = document.getElementById('dark-mode-toggle');
            const swapToggle = document.getElementById('dark-mode-toggle-swap');
            const navbarToggle = document.getElementById('dark-mode-toggle-navbar');
            if (toggle) {
              toggle.checked = newTheme === 'dark';
            }
            if (swapToggle) {
              swapToggle.checked = newTheme === 'dark';
            }
            if (navbarToggle) {
              navbarToggle.checked = newTheme === 'dark';
            }

            // Re-enable transitions after paint
            requestAnimationFrame(() => {
              document.documentElement.classList.remove('no-transition');
            });
          }
          
          // System theme detection - respect OS preference if user hasn't manually set
          (function() {
            const savedTheme = getCookie('theme');
            if (!savedTheme) {
              const prefersDark = window.matchMedia('(prefers-color-scheme: dark)').matches;
              document.body.setAttribute('data-theme', prefersDark ? 'dark' : 'light');
            }
            // Watch for OS theme changes (only if user hasn't manually set)
            window.matchMedia('(prefers-color-scheme: dark)').addEventListener('change', e => {
              if (!getCookie('theme')) {
                document.documentElement.classList.add('no-transition');
                document.body.setAttribute('data-theme', e.matches ? 'dark' : 'light');
                requestAnimationFrame(() => document.documentElement.classList.remove('no-transition'));
              }
            });
          })();

          // Initialize toggle state on page load
          window.addEventListener('DOMContentLoaded', function() {
            // Set initial toggle state based on data-theme attribute
            const currentTheme = document.body.getAttribute('data-theme');
            const toggle = document.getElementById('dark-mode-toggle');
            const swapToggle = document.getElementById('dark-mode-toggle-swap');
            const navbarToggle = document.getElementById('dark-mode-toggle-navbar');
            if (toggle) {
              toggle.checked = currentTheme === 'dark';
            }
            if (swapToggle) {
              swapToggle.checked = currentTheme === 'dark';
            }
            if (navbarToggle) {
              navbarToggle.checked = currentTheme === 'dark';
            }
          });
      |]
      let email = show $ maybe "" ((.persistentSession.user.getUser.email)) bcfg.sessM
      let name = maybe "" (\sess -> sess.persistentSession.user.getUser.firstName <> " " <> sess.persistentSession.user.getUser.lastName) bcfg.sessM
      let pidT = maybe "" (.id.toText) bcfg.currProject
      let pTitle = maybe "" (.title) bcfg.currProject
      let telemetryProjectId = bcfg.config.telemetryProjectId
      let telemetryServiceName = bcfg.config.telemetryServiceName
      script_
        [text| window.addEventListener("load", (event) => {
                  if (typeof posthog !== 'undefined' && posthog && posthog.people && posthog.people.set_once) {
                    posthog.people.set_once({email: ${email}, name: "${name}", projectId: "${pidT}", projectTitle: "${pTitle}"});
                  }
                  // echarts.connect('default');
                });
      |]
      -- Initialize Monoscope only when telemetryProjectId is available
      when (bcfg.config.telemetryProjectId /= "" && bcfg.config.enableBrowserMonitoring)
        $ let enableReplay = bool "false" "true" bcfg.config.enableSessionReplay
           in script_
                [text|
                  window.monoscope = new Monoscope({
                    projectId: "${telemetryProjectId}",
                    serviceName: "${telemetryServiceName}",
                    sessionReplay: ${enableReplay},
                    user: {
                      email: ${email},
                      name: "${name}"
                    }
                  });
              |]


projectsDropDown :: Projects.Project -> V.Vector Projects.Project -> Html ()
projectsDropDown currProject projects = do
  let pidTxt = currProject.id.toText
  div_
    [ term "data-menu" "true"
    , class_ "origin-top-right z-40 bg-bgOverlay p-2 absolute w-[18rem] rounded-xl shadow-lg border border-strokeWeak"
    ]
    do
      when (V.length projects > 1)
        $ div_ [class_ "p-1 pb-2"] do
          div_ [class_ "relative"] do
            div_ [class_ "absolute inset-y-0 left-0 pl-3 flex items-center pointer-events-none"] $ faSprite_ "magnifying-glass" "regular" "h-4 w-4 text-textWeak"
            input_
              [ type_ "search"
              , Aria.label_ "Search projects"
              , class_ "pl-10 w-full bg-fillWeak rounded-lg border-0 py-2 px-3 text-sm"
              , placeholder_ "Search..."
              , [__|on input
                  show .project_item in #projectsContainer when its textContent.toLowerCase() contains my value.toLowerCase()
                  then set visibleCount to #projectsContainer.querySelectorAll('.project_item:not([style*="display: none"])').length
                  if visibleCount == 0 remove .hidden from #noProjectsFound
                  else add .hidden to #noProjectsFound end|]
              ]
      div_ [class_ "space-y-0.5 max-h-[50vh] overflow-y-auto", id_ "projectsContainer"] do
        projects & mapM_ \project -> do
          let isActive = currProject.id == project.id
          let activeCls = if isActive then " bg-fillWeak font-medium" else " hover:bg-fillHover"
          a_ [class_ $ "flex justify-between items-center py-2 px-2.5 rounded-lg transition-colors duration-100 project_item min-w-0" <> activeCls, href_ $ "/p/" <> project.id.toText] do
            span_ [class_ "truncate"] $ toHtml project.title
            when isActive $ faSprite_ "check" "regular" "h-3.5 w-3.5 text-textBrand shrink-0"
        p_ [class_ "hidden text-textWeak text-sm text-center py-4", id_ "noProjectsFound"] "No matching projects"
      let actionLink attrs icon label = a_ (class_ "flex items-center gap-2 py-2 px-2.5 rounded-lg hover:bg-fillHover cursor-pointer text-sm" : attrs) $ faSprite_ icon "regular" "h-3.5 w-3.5 text-textWeak" >> span_ label
      div_ [class_ "border-t border-strokeWeak mt-1 pt-1"] do
        actionLink [href_ "/"] "grid" "All projects"
        actionLink [href_ "/p/new"] "plus" "New project"
        when (currProject.paymentPlan == "UsageBased" || currProject.paymentPlan == "GraduatedPricing")
          $ actionLink [hxGet_ [text| /p/$pidTxt/manage_subscription |]] "dollar-sign" "Manage billing"


sideNav :: Projects.Session -> Projects.Project -> Text -> Maybe Text -> Html ()
sideNav sess project pageTitle menuItem = aside_ [class_ "relative bg-fillWeaker max-md:bg-bgBase text-sm max-md:fixed max-md:z-50 max-md:w-60 max-md:h-full max-md:-translate-x-full max-md:transition-transform group-has-[#mobile-nav-toggle:checked]/pg:max-md:translate-x-0 md:min-w-15 md:shrink-0 md:w-15 group-has-[#sidenav-toggle:checked]/pg:md:w-60 h-screen md:transition-[width] duration-200 ease-out flex flex-col justify-between", id_ "side-nav-menu"] do
  -- Right border resize handle (desktop only)
  label_ [term "for" "sidenav-toggle", class_ "max-md:hidden absolute right-0 top-0 bottom-0 w-1 border-r border-strokeWeak cursor-e-resize group-has-[#sidenav-toggle:checked]/pg:cursor-w-resize hover:border-strokeBrand-strong hover:w-1 transition-colors z-10", Aria.label_ "Toggle sidebar"] ""
  div_ [class_ "px-2 group-has-[#sidenav-toggle:checked]/pg:px-3"] do
    input_ ([type_ "checkbox", class_ "hidden", id_ "sidenav-toggle", [__|on change call setCookie("isSidebarClosed", `${me.checked}`) then send "toggle-sidebar" to <body/>|]] <> [checked_ | sess.isSidebarClosed])
    div_ [class_ "pt-3.5 pb-5 flex justify-center group-has-[#sidenav-toggle:checked]/pg:justify-between items-center"] do
      -- Expanded: full logo
      a_ [href_ "/", class_ "relative h-6 flex-1 hidden group-has-[#sidenav-toggle:checked]/pg:inline-flex", Aria.label_ "Home"] do
        img_ [class_ "h-7 absolute inset-0 hidden group-has-[#sidenav-toggle:checked]/pg:block dark:hidden", src_ "/public/assets/svgs/logo_black.svg"]
        img_ [class_ "h-7 absolute inset-0 hidden group-has-[#sidenav-toggle:checked]/pg:dark:block", src_ "/public/assets/svgs/logo_white.svg"]
      -- Toggle sidebar (desktop: toggles sidenav-toggle, mobile: closes mobile-nav-toggle)
      label_ [term "for" "sidenav-toggle", class_ "max-md:hidden cursor-pointer text-strokeStrong min-w-[22px] min-h-[22px] flex items-center", Aria.label_ "Toggle sidebar", Aria.expanded_ (if sess.isSidebarClosed then "false" else "true"), Aria.controls_ "side-nav-menu", [__|on change from #sidenav-toggle if #sidenav-toggle.checked set @aria-expanded to 'false' else set @aria-expanded to 'true'|]] do
        faSprite_ "side-chevron-left-in-box" "regular" "h-5 w-5 rotate-180 group-has-[#sidenav-toggle:checked]/pg:rotate-0"
      label_ [term "for" "mobile-nav-toggle", class_ "md:!hidden max-md:flex cursor-pointer text-strokeStrong min-w-[22px] min-h-[22px] items-center", Aria.label_ "Close menu"] $ faSprite_ "side-chevron-left-in-box" "regular" "h-5 w-5 pointer-events-none"
    div_ [class_ "mt-4 dropdown block"] do
      a_
        [ class_ "flex flex-row text-textStrong hover:bg-fillWeak gap-2 items-center rounded-xl cursor-pointer py-2 justify-center group-has-[#sidenav-toggle:checked]/pg:py-3 group-has-[#sidenav-toggle:checked]/pg:px-3 group-has-[#sidenav-toggle:checked]/pg:border group-has-[#sidenav-toggle:checked]/pg:border-strokeWeak group-has-[#sidenav-toggle:checked]/pg:bg-fillWeaker transition-colors duration-100"
        , tabindex_ "0"
        , Aria.haspopup_ "listbox"
        , Aria.label_ $ "Switch project, current: " <> project.title
        , term "data-tippy-placement" "right"
        , term "data-tippy-content" $ project.title <> " — Switch project"
        ]
        do
          span_ [class_ "w-8 h-8 group-has-[#sidenav-toggle:checked]/pg:w-6 group-has-[#sidenav-toggle:checked]/pg:h-6 rounded-lg group-has-[#sidenav-toggle:checked]/pg:rounded-md bg-fillBrand-weak text-textBrand text-sm group-has-[#sidenav-toggle:checked]/pg:text-xs font-semibold flex items-center justify-center shrink-0"] $ toHtml $ T.take 1 project.title
          span_ [class_ "grow hidden group-has-[#sidenav-toggle:checked]/pg:block overflow-x-hidden whitespace-nowrap truncate"] $ toHtml project.title
          span_ [class_ "hidden group-has-[#sidenav-toggle:checked]/pg:flex shrink-0"] $ faSprite_ "angles-up-down" "regular" "w-4 text-textWeak"
      div_ [tabindex_ "0", class_ "dropdown-content z-40 group-has-[#sidenav-toggle:not(:checked)]/pg:left-full group-has-[#sidenav-toggle:not(:checked)]/pg:top-0 group-has-[#sidenav-toggle:not(:checked)]/pg:ml-2", role_ "listbox"] $ projectsDropDown project (Projects.getProjects $ Projects.projects sess.persistentSession)
    let mainNavActiveStyles = "[&_.main-nav-link.active]:bg-fillBrand-weak [&_.main-nav-link.active]:text-textStrong [&_.main-nav-link.active]:font-medium [&_.main-nav-link.active]:border-l-strokeBrand-strong [&_.main-nav-link.active]:border-y-transparent [&_.main-nav-link.active]:border-r-transparent [&_.main-nav-link.active_.nav-icon]:text-textBrand"
    nav_ [id_ "main-sidenav", class_ $ "mt-5 flex flex-col gap-1 text-textWeak " <> mainNavActiveStyles, [__|on click set #mobile-nav-toggle.checked to false end on htmx:pushedIntoHistory from window or popstate from window settle then set p to window.location.pathname then for link in .main-nav-link set h to link.getAttribute('href') if p is h or p.startsWith(h + '/') add .active to link else remove .active from link end end|]] do
      menu project.id & mapM_ \(mTitle, mUrl, fIcon) -> do
        let isActive = maybe (pageTitle == mTitle) (== mTitle) menuItem
        let activeCls = if isActive then " active" else ""
        a_
          ( [ href_ mUrl
            , term "data-tippy-placement" "right"
            , term "data-tippy-content" mTitle
            , class_ $ "main-nav-link relative group-has-[#sidenav-toggle:checked]/pg:px-4 gap-3 py-2 flex no-wrap shrink-0 justify-center group-has-[#sidenav-toggle:checked]/pg:justify-start items-center rounded-lg overflow-x-hidden overflow-y-hidden hover:bg-fillWeak hover:text-textStrong transition-colors duration-100" <> activeCls
            ]
              <> navTabAttrs
          )
          do
            faSprite_ fIcon "regular" "nav-icon w-4 h-4 shrink-0"
            span_ [class_ "hidden group-has-[#sidenav-toggle:checked]/pg:block whitespace-nowrap truncate"] $ toHtml mTitle
      onboardingChecklist_ project

  div_ [class_ "py-4 px-2 group-has-[#sidenav-toggle:checked]/pg:px-3 border-t border-strokeWeak flex flex-col gap-1"] do
    let currUser = sess.persistentSession.user.getUser
        userIdentifier =
          if currUser.firstName /= "" || currUser.lastName /= ""
            then currUser.firstName <> " " <> currUser.lastName
            else CI.original currUser.email
        avatarUrl = "/api/avatar/" <> currUser.id.toText

    -- Dark mode toggle
    -- Dark mode toggle
    -- Expanded: sun + toggle + moon
    label_ [class_ "hidden group-has-[#sidenav-toggle:checked]/pg:flex cursor-pointer gap-2 items-center px-2 py-2 rounded-lg hover:bg-fillWeak transition-colors duration-100", Aria.label_ "Toggle dark mode"] do
      faSprite_ "sun-bright" "regular" "h-4 w-4 text-textWeak"
      input_ [type_ "checkbox", class_ "toggle toggle-sm theme-controller", id_ "dark-mode-toggle", Aria.label_ "Toggle dark mode", onclick_ "toggleDarkMode()"]
      faSprite_ "moon-stars" "regular" "h-4 w-4 text-textWeak"
    -- Collapsed: centered icon button
    label_ [class_ "group-has-[#sidenav-toggle:checked]/pg:hidden flex justify-center items-center py-2 rounded-lg hover:bg-fillWeak cursor-pointer transition-colors duration-100", Aria.label_ "Toggle dark mode", term "data-tippy-placement" "right", term "data-tippy-content" "Toggle dark mode", onclick_ "toggleDarkMode()"] do
      span_ [class_ "dark:hidden"] $ faSprite_ "sun-bright" "regular" "h-4 w-4 text-textWeak"
      span_ [class_ "hidden dark:inline-flex"] $ faSprite_ "moon-stars" "regular" "h-4 w-4 text-textWeak"

    -- User avatar popover
    div_ [class_ "dropdown dropdown-top group-has-[#sidenav-toggle:checked]/pg:dropdown-top block group/user"] do
      div_
        [ tabindex_ "0"
        , role_ "button"
        , class_ "flex items-center gap-2 py-2 px-1 rounded-lg hover:bg-fillWeak cursor-pointer w-full justify-center group-has-[#sidenav-toggle:checked]/pg:justify-start"
        , Aria.haspopup_ "true"
        , Aria.label_ $ "User menu for " <> userIdentifier
        ]
        do
          img_ [class_ "w-8 h-8 rounded-full bg-fillPress shrink-0", src_ avatarUrl, term "data-tippy-placement" "right", term "data-tippy-content" userIdentifier]
          span_ [class_ "hidden group-has-[#sidenav-toggle:checked]/pg:flex items-center gap-1 overflow-hidden flex-1"] do
            span_ [class_ "truncate text-sm"] $ toHtml userIdentifier
            faSprite_ "chevron-down" "regular" "w-3 h-3 text-textWeak shrink-0 ml-auto transition-transform duration-150 rotate-180 group-focus-within/user:rotate-0"
      ul_ [tabindex_ "0", class_ "dropdown-content z-40 menu menu-md bg-bgOverlay rounded-box shadow-sm border border-strokeWeak w-56 mb-2", role_ "menu"] do
        div_ [class_ "px-3 py-2 text-sm"] do
          div_ [class_ "font-medium text-textStrong truncate"] $ toHtml userIdentifier
          div_ [class_ "text-textWeak text-xs truncate"] $ toHtml $ CI.original currUser.email
        div_ [class_ "divider my-0"] ""
        li_ [] $ a_ [href_ $ "/p/" <> project.id.toText <> "/settings", class_ "flex items-center gap-2"] do
          faSprite_ "gear" "regular" "w-4 h-4"
          "Settings"
        li_ [] $ a_ [href_ "https://monoscope.tech/docs/", target_ "blank", class_ "flex items-center gap-2"] do
          faSprite_ "circle-question" "regular" "w-4 h-4"
          "Documentation"
          faSprite_ "arrow-up-right-from-square" "regular" "w-3 h-3 text-textWeak ml-auto"
        div_ [class_ "divider my-0"] ""
        li_ [] $ a_ [href_ "/logout", class_ "flex items-center gap-2 text-textError", [__| on click js posthog.reset(); end |]] do
          faSprite_ "arrow-right-from-bracket" "regular" "w-4 h-4"
          "Logout"


-- mapM_ renderNavBottomItem $ navBottomList project.id.toText

-- | Floating command palette trigger for pages without navbar (settings, etc.)
paletteTriggerFloating :: Projects.Project -> Html ()
paletteTriggerFloating p =
  button_
    [ class_ "fixed top-14 right-4 btn btn-sm btn-ghost bg-base-200 gap-1.5 text-textWeak z-50"
    , data_ "palette-url" ("/p/" <> p.id.toText <> "/command-palette")
    , [__|on click
          if <.cmd-palette-backdrop/> exists
            remove <.cmd-palette-backdrop/>
          else
            set :url to my.dataset.paletteUrl
            fetch `${:url}` then put the result at start of <body/>
          end
        end|]
    ]
    do
      faSprite_ "magnifying-glass" "regular" "w-3.5 h-3.5"
      kbd_ [class_ "kbd kbd-xs"] "\x2318K"


navbar :: Maybe Projects.Project -> [(Text, Text, Text)] -> Projects.User -> Maybe Text -> Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe (Html ()) -> Maybe (Html ()) -> Html ()
navbar projectM menuL currUser prePageTitle pageTitle pageTitleSuffix pageTitleMonadId pageTitleSuffixModalId docsLink tabsM pageActionsM =
  nav_ [id_ "main-navbar", class_ "w-full max-md:px-2 max-md:py-1.5 px-4 py-2 flex flex-row flex-wrap border-strokeWeak items-center"] do
    div_ [class_ "flex-1 flex items-center text-textStrong gap-1 min-w-0 overflow-hidden"] do
      whenJust projectM \_ -> do
        div_ [class_ "md:!hidden max-md:flex group-has-[#mobile-nav-toggle:checked]/pg:max-md:!hidden cursor-pointer text-strokeStrong p-2 -m-2 items-center justify-center", Aria.label_ "Open menu", [__|on click set #mobile-nav-toggle.checked to true|]] $ faSprite_ "side-chevron-left-in-box" "regular" "h-5 w-5 rotate-180 pointer-events-none"
        div_ [class_ "md:!hidden max-md:block group-has-[#mobile-nav-toggle:checked]/pg:max-md:!hidden w-px h-5 bg-strokeWeak ml-2"] ""
      whenJust prePageTitle \pt -> whenJust (find (\a -> fst3 a == pt) menuL) \(_, url, icon) -> do
        a_ ([class_ "max-md:hidden p-1 hover:bg-fillWeak inline-flex items-center justify-center gap-1 rounded-md text-sm", href_ url] <> navTabAttrs) do
          faSprite_ icon "regular" "w-4 h-4 text-strokeStrong"
          toHtml pt
        faSprite_ "chevron-right" "regular" "w-3 h-3 max-md:hidden"
      let targetPage = Components.getTargetPage pageTitle
          titleBase = "font-normal text-xl max-md:text-base p-1 rounded-md leading-none truncate"
      if targetPage /= "" && isJust pageTitleSuffix
        then whenJust projectM \p -> a_ ([class_ $ titleBase <> " cursor-pointer hover:bg-fillWeak", href_ $ "/p/" <> p.id.toText <> targetPage, id_ "pageTitleText"] <> navTabAttrs) $ toHtml pageTitle
        else label_ [class_ $ titleBase <> " cursor-pointer hover:bg-fillWeak", Lucid.for_ $ maybeToMonoid pageTitleMonadId, id_ "pageTitleText"] $ toHtml pageTitle
      -- Show tab/suffix in breadcrumbs if present (with ID for htmx out-of-band updates)
      span_ [id_ "pageTitleSuffix", class_ "max-md:hidden flex items-center gap-1"] $ whenJust pageTitleSuffix \suffix -> do
        faSprite_ "chevron-right" "regular" "w-3 h-3"
        -- Make tab name clickable if modal ID is provided
        case pageTitleSuffixModalId of
          Just modalId -> label_ [class_ "font-normal text-xl p-1 leading-none text-textWeak cursor-pointer hover:bg-fillWeak rounded-md", Lucid.for_ modalId, id_ "pageTitleSuffixText"] $ toHtml suffix
          Nothing -> span_ [class_ "font-normal text-xl p-1 leading-none text-textWeak", id_ "pageTitleSuffixText"] $ toHtml suffix
      whenJust docsLink \link -> a_ [class_ "max-md:hidden text-iconBrand -mt-1", href_ link, term "data-tippy-placement" "right", term "data-tippy-content" "Open Documentation"] $ faSprite_ "circle-question" "regular" "w-4 h-4"
    whenJust tabsM $ div_ [class_ $ bool "" "max-md:order-last max-md:w-full max-md:pt-1" (isJust pageActionsM)]
    div_ [class_ $ "flex-1 flex items-center justify-end gap-2 text-sm" <> maybe " max-md:hidden" (const "") pageActionsM] do
      -- Command palette trigger
      whenJust projectM \p ->
        button_
          [ id_ "cmd-palette-trigger"
          , class_ "btn btn-ghost btn-sm gap-1.5 text-textWeak max-md:hidden"
          , data_ "palette-url" ("/p/" <> p.id.toText <> "/command-palette")
          , [__|on click
                if <.cmd-palette-backdrop/> exists
                  remove <.cmd-palette-backdrop/>
                else
                  set :url to my.dataset.paletteUrl
                  fetch `${:url}` then put the result at start of <body/>
                end
              end|]
          ]
          do
            faSprite_ "magnifying-glass" "regular" "w-3.5 h-3.5"
            kbd_ [class_ "kbd kbd-xs"] "\x2318K"
      whenJust pageActionsM id


alerts_ :: Html ()
alerts_ = do
  template_ [id_ "successToastTmpl"] do
    div_ [role_ "alert", class_ "alert alert-success max-md:w-full md:w-96 cursor-pointer toast-animate", [__|init wait for click or 30s then transition my opacity to 0 then remove me|]] do
      faSprite_ "circle-check" "solid" "stroke-current shrink-0 w-6 h-6"
      span_ [class_ "title"] "Something succeeded"
  template_ [id_ "errorToastTmpl"] do
    div_ [role_ "alert", class_ "alert alert-error max-md:w-full md:w-96 cursor-pointer toast-animate", [__|init wait for click or 30s then transition my opacity to 0 then remove me|]] do
      faSprite_ "circle-exclamation" "solid" "stroke-current shrink-0 w-6 h-6"
      span_ [class_ "title"] "Something failed"
  section_ [class_ "fixed top-0 right-0 z-50 pt-14 pr-5 max-md:left-0 max-md:px-4 space-y-3 pointer-events-none [&>*]:pointer-events-auto", id_ "toastsParent"] ""
  script_
    [type_ "text/javascript"]
    [text|
    document.addEventListener('DOMContentLoaded', function(){
      document.body.addEventListener('triggerToast', function(e){
          e.detail.value.forEach(function(toastEvent){
            const template = document.getElementById(toastEvent[0].toLowerCase()+'ToastTmpl');
            const clone = document.importNode(template.content, true);
            clone.querySelector('.title').textContent = toastEvent[1];
            document.getElementById("toastsParent").appendChild(clone);
            _hyperscript.processNode(document.querySelector("#toastsParent"));
         })
      })
    })
  |]


loginBanner :: Html ()
loginBanner = do
  div_ [class_ "flex items-center justify-between border-b border-strokeWeak bg-fillWeak max-md:px-2 px-4 py-1.5 gap-2 text-sm max-md:text-xs max-md:flex-wrap"] do
    div_ [class_ "flex items-center gap-2"] do
      faSprite_ "flask" "regular" "h-4 w-4 text-iconBrand"
      span_ [class_ "font-medium text-textStrong"] "Demo Project"
      span_ [class_ "hidden sm:inline text-textWeak"] "· Explore Monoscope's features"
    div_ [class_ "flex items-center gap-2 max-md:gap-1.5 max-md:ml-auto"] do
      a_ [class_ "text-textBrand hover:underline underline-offset-2 max-md:hidden", href_ "https://monoscope.tech/docs/onboarding/"] "Docs"
      a_ [class_ "py-1 px-2.5 rounded-lg bg-fillWeak hover:bg-fillHover text-textStrong border border-strokeWeak text-xs font-medium max-md:hidden", href_ "https://calendar.app.google/1a4HG5GZYv1sjjZG6"] "Book Demo"
      a_ [class_ "py-1 px-2.5 rounded-lg bg-fillBrand-strong hover:opacity-90 text-textInverse-strong text-xs font-medium", href_ "/login"] "Start Free Trial"


settingsWrapper :: Projects.ProjectId -> Text -> Html () -> Html ()
settingsWrapper pid current pageHtml = do
  let
    navActiveStyles = "[&_.settings-nav-link]:hover:bg-fillWeak [&_.settings-nav-link]:text-textWeak [&_.settings-nav-link.active]:bg-fillBrand-weak [&_.settings-nav-link.active]:text-textBrand [&_.settings-nav-link.active]:hover:bg-fillBrand-weak"
  section_ [class_ "flex max-md:flex-col h-full w-full"] do
    nav_ [id_ "settings-nav", class_ "md:w-52 shrink-0 md:h-full max-md:px-3 max-md:py-2.5 p-4 md:pt-8 max-md:border-b max-md:border-b-strokeWeak md:border-r md:border-r-strokeWeak max-md:overflow-x-auto max-md:scrollbar-hide", term "preload" "mouseover"] do
      h1_ [class_ "text-lg pl-3 font-semibold text-textStrong max-md:hidden"] "Settings"
      ul_ [class_ $ "flex max-md:flex-row max-md:flex-nowrap md:flex-col md:mt-4 gap-0.5 w-full " <> navActiveStyles] do
        li_ [class_ "md:hidden shrink-0"]
          $ div_ [class_ "flex items-center px-2.5 py-2 rounded-lg cursor-pointer text-strokeStrong hover:bg-fillWeak", Aria.label_ "Open menu", [__|on click set #mobile-nav-toggle.checked to true|]]
          $ faSprite_ "side-chevron-left-in-box" "regular" "shrink-0 h-4.5 w-4.5 rotate-180"
        mapM_ (renderNavBottomItem current) $ navBottomList pid.toText
    main_ [id_ "settings-content", class_ "relative w-full h-full overflow-y-auto"] do
      div_ [id_ settingsLoadingId, class_ "htmx-indicator absolute inset-0 z-10 bg-bgBase/60 flex items-center justify-center"] do
        loadingIndicatorWith_ LdMD LdSpinner "text-textBrand"
      pageHtml


navBottomList :: Text -> [(Text, Text, Text)]
navBottomList pidTxt =
  [ ("gear", "General", "/p/" <> pidTxt <> "/settings")
  , ("key", "API Keys", "/p/" <> pidTxt <> "/apis")
  , ("users", "Team", "/p/" <> pidTxt <> "/manage_members")
  , ("arrows-turn-right", "Integrations", "/p/" <> pidTxt <> "/integrations")
  , ("dollar", "Billing", "/p/" <> pidTxt <> "/manage_billing")
  ]


settingsContentTarget :: Text
settingsContentTarget = "#settings-content"


settingsLoadingId :: Text
settingsLoadingId = "settings-loading"


renderNavBottomItem :: Text -> (Text, Text, Text) -> Html ()
renderNavBottomItem curr (iconName, linkText, link) =
  li_ [] do
    a_
      [ class_ $ "settings-nav-link flex gap-2 md:gap-3 items-center px-2.5 md:px-3 py-2 rounded-lg whitespace-nowrap" <> if curr == linkText then " active" else ""
      , term "data-tippy-placement" "right"
      , term "data-tippy-content" linkText
      , href_ link
      , hxGet_ link
      , hxTarget_ settingsContentTarget
      , hxSelect_ settingsContentTarget
      , term "hx-select-oob" "#settings-nav:morph"
      , hxSwap_ "morph"
      , hxPushUrl_ "true"
      , hxIndicator_ ("#" <> settingsLoadingId)
      , [__|on click set my.preloadState to 'DONE'|]
      ]
      do
        faSprite_ iconName "regular" "shrink-0 h-4 w-4"
        span_ [class_ "text-sm font-medium"] (toHtml linkText)


externalHeadScripts_ :: EnvConfig -> Html ()
externalHeadScripts_ config = do
  -- Google Ads
  whenJust config.googleAdsConversionId $ \conversionId -> do
    script_ [async_ "true", src_ $ "https://www.googletagmanager.com/gtag/js?id=" <> conversionId] ("" :: Text)
    script_
      [fmt|
            window.dataLayer = window.dataLayer || [];
            function gtag(){{dataLayer.push(arguments);}}
            gtag('js', new Date());
            gtag('config', '{conversionId}');

            function gtag_report_conversion(url) {{
              var callback = function () {{
                if (typeof(url) != 'undefined') {{
                  window.location = url;
                }}
              }};
              gtag('event', 'conversion', {{
                  'send_to': '{conversionId}/IUBqCKOA-8sYEIvoroUq',
                  'event_callback': callback
              }});
              return false;
            }} |]

  -- Facebook Pixel Code
  when (isJust config.facebookPixelId1 || isJust config.facebookPixelId2) $ do
    let pixelInitScript =
          mconcat
            $ catMaybes
              [ config.facebookPixelId1 <&> \pixelId -> [fmt|fbq('init', '{pixelId}'); fbq('track', 'PageView');|]
              , config.facebookPixelId2 <&> \pixelId -> [fmt|fbq('init', '{pixelId}'); fbq('track', 'PageView');|]
              ]
    script_
      [fmt|
          setTimeout(function(){{
      !function(f,b,e,v,n,t,s)
    {{if(f.fbq)return;n=f.fbq=function(){{n.callMethod?
    n.callMethod.apply(n,arguments):n.queue.push(arguments)}};
    if(!f._fbq)f._fbq=n;n.push=n;n.loaded=!0;n.version='2.0';
    n.queue=[];t=b.createElement(e);t.async=!0;
    t.src=v;s=b.getElementsByTagName(e)[0];
    s.parentNode.insertBefore(t,s)}}(window,document,'script',
    'https://connect.facebook.net/en_US/fbevents.js');
      {pixelInitScript}
      }},3000);
      |]
    whenJust config.facebookPixelId2 $ \pixelId ->
      noscript_ $ img_ [height_ "1", width_ "1", src_ $ "https://www.facebook.com/tr?id=" <> pixelId <> "&ev=PageView&noscript=1"]
  -- End Facebook Pixel Code

  -- Google Tag Manager
  whenJust config.googleTagManagerId $ \gtmId -> do
    script_
      [fmt|
    (function(w,d,s,l,i){{w[l]=w[l]||[];w[l].push({{'gtm.start':
    new Date().getTime(),event:'gtm.js'}});var f=d.getElementsByTagName(s)[0],
    j=d.createElement(s),dl=l!='dataLayer'?'&l='+l:'';j.async=true;j.src=
    'https://www.googletagmanager.com/gtm.js?id='+i+dl;f.parentNode.insertBefore(j,f);
    }})(window,document,'script','dataLayer','{gtmId}');
      |]
    noscript_ $ iframe_ [height_ "0", width_ "0", style_ "display:none;visibility:hidden", src_ $ "https://www.googletagmanager.com/ns.html?id=" <> gtmId] ""
  -- End Google Tag Manager

  -- LinkedIn pixel
  whenJust config.linkedInPartnerId $ \partnerId -> do
    script_
      [fmt|
    _linkedin_partner_id = "{partnerId}"; window._linkedin_data_partner_ids = window._linkedin_data_partner_ids || [];
    window._linkedin_data_partner_ids.push(_linkedin_partner_id);
      |]
    script_
      [raw|
    setTimeout(function(){
    (function(l) { if (!l){window.lintrk = function(a,b){window.lintrk.q.push([a,b])}; window.lintrk.q=[]} var s = document.getElementsByTagName("script")[0]; var b = document.createElement("script"); b.type = "text/javascript";b.async = true; b.src = "https://snap.licdn.com/li.lms-analytics/insight.min.js"; s.parentNode.insertBefore(b, s);})(window.lintrk);
                  },3000);
      |]
    noscript_ $ img_ [height_ "0", width_ "0", style_ "display:none;visibility:hidden", src_ $ "https://px.ads.linkedin.com/collect/?pid=" <> partnerId <> "&fmt=gif"]
  -- End LinkedIn

  -- PostHog
  whenJust config.postHogApiKey $ \apiKey -> do
    let apiHost = fromMaybe "https://eu.i.posthog.com" config.postHogApiHost
    script_
      [fmt|
(function() {{
    !function(t,e){{var o,n,p,r;e.__SV||(window.posthog=e,e._i=[],e.init=function(i,s,a){{function g(t,e){{var o=e.split(".");2==o.length&&(t=t[o[0]],e=o[1]),t[e]=function(){{t.push([e].concat(Array.prototype.slice.call(arguments,0)))}}}}(p=t.createElement("script")).type="text/javascript",p.crossOrigin="anonymous",p.async=!0,p.src=s.api_host.replace(".i.posthog.com","-assets.i.posthog.com")+"/static/array.js",(r=t.getElementsByTagName("script")[0]).parentNode.insertBefore(p,r);var u=e;for(void 0!==a?u=e[a]=[]:a="posthog",u.people=u.people||[],u.toString=function(t){{var e="posthog";return"posthog"!==a&&(e+="."+a),t||(e+=" (stub)"),e}},u.people.toString=function(){{return u.toString(1)+".people (stub)"}},o="init Ce Ls Ns Te As js capture Xe calculateEventProperties qs register register_once register_for_session unregister unregister_for_session Gs getFeatureFlag getFeatureFlagPayload isFeatureEnabled reloadFeatureFlags updateEarlyAccessFeatureEnrollment getEarlyAccessFeatures on onFeatureFlags onSurveysLoaded onSessionId getSurveys getActiveMatchingSurveys renderSurvey canRenderSurvey canRenderSurveyAsync identify setPersonProperties group resetGroups setPersonPropertiesForFlags resetPersonPropertiesForFlags setGroupPropertiesForFlags resetGroupPropertiesForFlags reset get_distinct_id getGroups get_session_id get_session_replay_url alias set_config startSessionRecording stopSessionRecording sessionRecordingStarted captureException loadToolbar get_property getSessionProperty Hs Us createPersonProfile Ws Os Js opt_in_capturing opt_out_capturing has_opted_in_capturing has_opted_out_capturing get_explicit_consent_status is_capturing clear_opt_in_out_capturing zs debug L Bs getPageViewId captureTraceFeedback captureTraceMetric".split(" "),n=0;n<o.length;n++)g(u,o[n]);e._i.push([i,s,a])}},e.__SV=1)}}(document,window.posthog||[]);
    posthog.init('{apiKey}', {{
        api_host: '{apiHost}',
        defaults: '2025-05-24',
        person_profiles: 'identified_only'
    }})
}})();
        |]
  -- Crisp chat
  whenJust config.crispWebsiteId $ \websiteId ->
    script_
      [fmt|window.$crisp = []; window.CRISP_WEBSITE_ID = "{websiteId}"; (function () {{ d = document; s = d.createElement("script"); s.src = "https://client.crisp.chat/l.js"; s.async = 1; d.getElementsByTagName("head")[0].appendChild(s); }})();|]
