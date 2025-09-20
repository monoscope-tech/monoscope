{-# LANGUAGE PackageImports #-}

module Pages.BodyWrapper (bodyWrapper, BWConfig (..), PageCtx (..)) where

import Data.CaseInsensitive qualified as CI
import Data.Default (Default)
import Data.Text qualified as T
import Data.Tuple.Extra (fst3)
import Data.Vector qualified as V
import Lucid
import Lucid.Htmx (hxGet_, hxSelect_, hxSwap_, hxTarget_, hxTrigger_)
import Lucid.Hyperscript (__)
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import Models.Users.Users qualified as Users
import NeatInterpolation (text)
import Pages.Components qualified as Components
import Pkg.Components.ExternalHeadScripts (externalHeadScripts_)
import Pkg.Components.Modals qualified as Components
import Pkg.THUtils
import PyF
import Relude
import System.Config (EnvConfig (..))
import Utils (faSprite_, freeTierLimitExceededBanner)
import "cryptohash-md5" Crypto.Hash.MD5 qualified as MD5


menu :: Projects.ProjectId -> [(Text, Text, Text)]
menu pid =
  [ ("Dashboards", "/p/" <> pid.toText <> "/dashboards", "dashboard")
  , ("Explorer", "/p/" <> pid.toText <> "/log_explorer", "explore")
  , ("API Catalog", "/p/" <> pid.toText <> "/api_catalog", "swap")
  , ("Changes & Errors", "/p/" <> pid.toText <> "/anomalies", "bug")
  , ("Alerts", "/p/" <> pid.toText <> "/monitors", "list-check")
  , ("Reports", "/p/" <> pid.toText <> "/reports", "chart-simple")
  ]


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
  { sessM :: Maybe Sessions.Session
  , currProject :: Maybe Projects.Project
  , prePageTitle :: Maybe Text
  , pageTitle :: Text
  , pageTitleModalId :: Maybe Text --
  , menuItem :: Maybe Text -- Use PageTitle if menuItem is not set
  , navTabs :: Maybe (Html ())
  , pageActions :: Maybe (Html ())
  , docsLink :: Maybe Text
  , isSettingsPage :: Bool
  , freeTierExceeded :: Bool
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
        link_ [rel_ "stylesheet", type_ "text/css", href_ $(hashAssetFile "/public/assets/css/thirdparty/notyf3.min.css")]
        link_ [rel_ "stylesheet", href_ "https://cdn.jsdelivr.net/npm/@yaireo/tagify/dist/tagify.css", type_ "text/css"]
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
        script_ [src_ $(hashAssetFile "/public/assets/deps/htmx/htmx-2.js")] ("" :: Text)
        script_ [src_ $(hashAssetFile "/public/assets/deps/htmx/multi-swap.js"), defer_ "true"] ("" :: Text)
        script_ [src_ $(hashAssetFile "/public/assets/deps/htmx/preload.js"), defer_ "true"] ("" :: Text)
        script_ [src_ $(hashAssetFile "/public/assets/deps/htmx/json-enc-2.js"), defer_ "true"] ("" :: Text)
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

        script_ [src_ "https://unpkg.com/@monoscopetech/browser@latest/dist/monoscope.min.js"] ("" :: Text)

        script_ [type_ "module", src_ $(hashAssetFile "/public/assets/web-components/dist/js/index.js")] ("" :: Text)

        script_
          [text|
      !function(e,t,n,s,u,a){e.twq||(s=e.twq=function(){s.exe?s.exe.apply(s,arguments):s.queue.push(arguments);},s.version='1.1',s.queue=[],u=t.createElement(n),u.async=!0,u.src='https://static.ads-twitter.com/uwt.js',
      a=t.getElementsByTagName(n)[0],a.parentNode.insertBefore(u,a))}(window,document,'script');
      twq('config','om5gt');
      |]

        script_
          [text|
          function getTags() {
            console.log("here")
            const tag = window.tagify.value
            console.log(tag)
            const values = tag.map(tag => tag.value);
            console.log(values)
            return values || []
          }
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


        function navigatable(me, target, container, activeClass, excl)  {
            const exCls = excl ? ":not(" + excl + ".a-tab)" : "";
            const exClsC = excl ? ":not(" + excl + ".a-tab-content)" : "";
            const nav = document.querySelector(container);
            const tabs = nav.querySelectorAll(".a-tab" + exCls);
            const contents = nav.querySelectorAll(".a-tab-content" + exClsC);
            const targetElement = document.querySelector(target);
            
            // Batch DOM updates using requestAnimationFrame
            requestAnimationFrame(() => {
              tabs.forEach(tab => {
                tab.classList.remove(activeClass);
              });
              me.classList.add(activeClass);
              contents.forEach(content => content.classList.add("hidden"));
              targetElement.classList.remove("hidden");
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
          // Lazy tooltip initialization for better performance
          function initTooltips() {
            document.querySelectorAll('[data-tippy-content]:not([data-tippy-initialized])').forEach(element => {
              element.setAttribute('data-tippy-initialized', 'true');
              
              // Add mouseenter listener only once per element
              element.addEventListener('mouseenter', function() {
                if (!element._tippy) {
                  const instance = tippy(element, {
                    delay: [100, 0],
                    duration: 0,
                    updateDuration: 0,
                    animateFill: false,
                    moveTransition: '',
                    animation: false,
                    touch: false,
                    followCursor: false,
                    flipOnUpdate: false,
                    lazy: true,
                    popperOptions: {
                        strategy: 'absolute',  // Required for scrolling containers
                        
                        modifiers: [
                          {
                            name: 'computeStyles',
                            options: {
                              gpuAcceleration: true,  // Still use GPU acceleration!
                              adaptive: false,         // Reduce style recalculations
                            },
                          },
                        ],
                      },
                  });
                  // Show tooltip immediately on first hover
                  instance.show();
                }
              }, { once: true }); // Listener removes itself after first trigger
            });
          }
          
          // Initialize tooltips for current elements
          initTooltips();
          
          // Re-initialize for dynamically added content
          document.body.addEventListener('htmx:afterSwap', initTooltips);
          document.body.addEventListener('htmx:afterSettle', initTooltips);
          
          var notyf = new Notyf({
              duration: 5000,
              position: {x: 'right', y: 'top'},
          });
          document.body.addEventListener("successToast", (e)=> {e.detail.value.map(v=>notyf.success(v));});
          document.body.addEventListener("errorToast", (e)=> {e.detail.value.map(v=>notyf.error(v));});
        });
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
                    send successToast(value:['Value copied to the Clipboard']) to <body/>
                    halt
              end
            end
    |]

    body_ [class_ "h-full w-full bg-bgBase text-textStrong group/pg", term "data-theme" (maybe "dark" (.theme) bcfg.sessM), term "hx-ext" "multi-swap,preload", term "preload" "mouseover"] do
      div_
        [ style_ "z-index:99999"
        , class_ "pt-24 sm:hidden justify-center z-50 w-full p-4 bg-fillWeak overflow-y-auto inset-0 h-full max-h-full"
        , tabindex_ "-1"
        ]
        do
          div_ [class_ "relative mx-auto max-h-full", style_ "width: min(90vw, 500px)"]
            $ div_ [class_ "bg-base-100 rounded-lg drop-shadow-md border-1 w-full"] do
              div_ [class_ "flex items-start justify-between p-6 space-x-2  border-b rounded-t"] do
                h3_ [class_ "text-3xl font-bold "] "Only Desktop Browsers are Supported for now!"
              -- Modal body
              div_ [class_ "w-full"] $ div_ [class_ "p-6 space-y-6", style_ "height:50vh; width:100%"] do
                p_ [class_ ""] "Due to the heavy visualization usecases we're solving, APItoolkit is not supported on mobile, and can only be used from a desktop browser at the moment."
                p_ [class_ ""] "We're diligently working on expanding its availability to other platforms, and we'll keep you updated as we make progress. "
                p_ [] "Don't hesitate to let us know if this is a very important feature for your team, then we can prioritize it"
              -- Modal footer
              div_ [class_ "flex w-full justify-end items-center p-6 space-x-2 border-t border-strokeMedium rounded-b"] pass
      case bcfg.sessM of
        Nothing -> do
          section_ [class_ "flex flex-col grow  h-screen overflow-y-hidden"]
            $ section_
              [class_ "flex-1 overflow-y-auto"]
              child
        Just sess ->
          let currUser = sess.persistentSession.user.getUser
              sideNav' = bcfg.currProject & maybe "" \project -> sideNav sess project (fromMaybe bcfg.pageTitle bcfg.prePageTitle) bcfg.menuItem
           in section_ [class_ "flex flex-row grow-0 h-screen overflow-hidden"] do
                sideNav'
                section_ [class_ "h-full overflow-y-hidden grow flex flex-col"] do
                  when
                    (currUser.email == "hello@apitoolkit.io")
                    loginBanner
                  unless (bcfg.isSettingsPage || bcfg.hideNavbar) $ navbar bcfg.currProject (maybe [] (\p -> menu p.id) bcfg.currProject) currUser bcfg.prePageTitle bcfg.pageTitle bcfg.pageTitleModalId bcfg.docsLink bcfg.navTabs bcfg.pageActions
                  section_ [class_ "overflow-y-hidden h-full grow"] do
                    when bcfg.freeTierExceeded $ whenJust bcfg.currProject (\p -> freeTierLimitExceededBanner p.id.toText)
                    if bcfg.isSettingsPage
                      then maybe child (\p -> settingsWrapper p.id bcfg.pageTitle child) bcfg.currProject
                      else child
                  div_ [class_ "h-0 shrink"] do
                    Components.drawer_ "global-data-drawer" Nothing Nothing ""
                    -- Modal for copying widgets to other dashboards
                    Components.modal_ "dashboards-modal" "" do
                      -- Hidden fields to store widget and dashboard IDs
                      input_ [type_ "hidden", id_ "dashboards-modal-widget-id", name_ "widget_id"]
                      input_ [type_ "hidden", id_ "dashboards-modal-source-dashboard-id", name_ "source_dashboard_id"]

                      div_
                        [ class_ "dashboards-list space-y-3 max-h-160 overflow-y-auto"
                        , hxGet_ ("/p/" <> maybe "" (.id.toText) bcfg.currProject <> "/dashboards?embedded=true")
                        , hxTrigger_ "intersect once"
                        , hxSelect_ "#itemsListPage"
                        , hxSwap_ "innerHTML"
                        ]
                        do
                          div_ [class_ "skeleton h-16 w-full"] ""
                          div_ [class_ "skeleton h-16 w-full"] ""
                          div_ [class_ "skeleton h-16 w-full"] ""

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

          
          // Dark mode toggle function
          function toggleDarkMode() {
            const currentTheme = document.body.getAttribute('data-theme');
            const newTheme = currentTheme === 'dark' ? 'light' : 'dark';
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
          }
          
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
      script_
        [text| window.addEventListener("load", (event) => {
                  if (typeof posthog !== 'undefined' && posthog && posthog.people && posthog.people.set_once) {
                    posthog.people.set_once({email: ${email}, name: "${name}", projectId: "${pidT}", projectTitle: "${pTitle}"});
                  }
                  // echarts.connect('default');
                });
      |]
      -- Initialize Monoscope only when telemetryProjectId is available
      when (bcfg.config.telemetryProjectId /= "")
        $ script_
          [text| window.addEventListener("load", (event) => {
          window.monoscope = new Monoscope({ 
            projectId: "${bcfg.envConfig.telemetryProjectId}", 
            serviceName: "${bcfg.envConfig.telemetryServiceName}", 
            user: {
              email: ${email}, 
              name: "${name}"
            }
          });
        });
        |]


projectsDropDown :: Projects.Project -> V.Vector Projects.Project -> Html ()
projectsDropDown currProject projects = do
  let pidTxt = currProject.id.toText
  div_
    [ term "data-menu" "true"
    , class_ "origin-top-right z-40 transition transform bg-bgOverlay p-4 absolute w-[20rem] rounded-2xl shadow-2xl shadow-strokeBrand-weak opacity-100 scale-100"
    ]
    do
      div_ [class_ "p-2 pb-4 "] do
        div_ [class_ "flex mt-2 mb-4"] do
          faSprite_ "folders" "regular" "h-5 w-5 mr-2"
          div_ do
            strong_ [class_ "block"] $ toHtml currProject.title
            small_ [class_ "block"] $ toHtml currProject.paymentPlan
        nav_ [] do
          when (currProject.paymentPlan == "UsageBased" || currProject.paymentPlan == "GraduatedPricing")
            $ a_
              [class_ "p-3 flex gap-3 items-center rounded-sm hover:bg-fillHover cursor-pointer", hxGet_ [text| /p/$pidTxt/manage_subscription |]]
              (faSprite_ "dollar-sign" "regular" "h-5 w-5" >> span_ "Manage billing")
      div_ [class_ "border-t border-strokeWeak p-2"] do
        div_ [class_ "flex justify-between content-center items-center py-5 mb-2 "] do
          a_ [href_ "/"] $ h3_ [] "Switch projects"
          a_ [class_ "bg-fillBrand-strong flex pl-3 pr-4 py-2 rounded-xl text-textInverse-strong space-x-2", href_ "/p/new"] do
            faSprite_ "plus" "regular" "h-5 w-5 bg-fillBrand-strong rounded-xl" >> span_ [class_ "inline-block px-1"] "Add"
        div_ do
          div_ [class_ "relative"] do
            div_ [class_ "absolute inset-y-0 left-0 pl-4 flex items-center pointer-events-none"] $ faSprite_ "magnifying-glass" "regular" "h-6 w-4"
            input_
              [ class_ "pl-12 w-full  bg-fillWeak rounded-2xl border-0 p-3"
              , placeholder_ "Search Projects"
              , [__|on input show .project_item in #projectsContainer when its textContent.toLowerCase() contains my value.toLowerCase()|]
              ]
          div_ [class_ "space-y-2 py-4 ", id_ "projectsContainer"] do
            projects & mapM_ \project ->
              a_ [class_ "flex justify-between p-2 project_item", href_ $ "/p/" <> project.id.toText] do
                div_ [class_ "space-x-3"] (faSprite_ "folders" "regular" "h-5 w-5 inline-block" >> span_ [class_ "inline-block"] (toHtml project.title))
                when (currProject.id == project.id) $ faSprite_ "circle-check" "regular" "h-6 w-6 text-textSuccess"


sideNav :: Sessions.Session -> Projects.Project -> Text -> Maybe Text -> Html ()
sideNav sess project pageTitle menuItem = aside_ [class_ "border-r bg-fillWeaker border-strokeWeak text-sm min-w-15 shrink-0 w-15 group-has-[#sidenav-toggle:checked]/pg:w-60  h-screen transition-all duration-200 ease-in-out flex flex-col justify-between", id_ "side-nav-menu"] do
  div_ [class_ "px-2 group-has-[#sidenav-toggle:checked]/pg:px-3"] do
    div_ [class_ "py-5 flex justify-center group-has-[#sidenav-toggle:checked]/pg:justify-between items-center"] do
      a_ [href_ "/", class_ "relative h-6 flex-1 hidden group-has-[#sidenav-toggle:checked]/pg:inline-flex"] do
        -- Full logos (shown when sidebar is expanded)
        img_ [class_ "h-7 absolute inset-0 hidden group-has-[#sidenav-toggle:checked]/pg:block dark:hidden", src_ "/public/assets/svgs/logo_black.svg"]
        img_ [class_ "h-7 absolute inset-0 hidden group-has-[#sidenav-toggle:checked]/pg:dark:block", src_ "/public/assets/svgs/logo_white.svg"]
      label_ [class_ "cursor-pointer text-strokeStrong"] do
        input_ ([type_ "checkbox", class_ "hidden", id_ "sidenav-toggle", [__|on change call setCookie("isSidebarClosed", `${me.checked}`)|]] <> [checked_ | sess.isSidebarClosed])
        faSprite_ "side-chevron-left-in-box" "regular" " h-5 w-5 rotate-180 group-has-[#sidenav-toggle:checked]/pg:rotate-0"
    div_ [class_ "mt-4 sd-px-0 dropdown block"] do
      a_
        [ class_ "flex flex-row border border-strokeWeak bg-fillWeaker text-textStrong hover:bg-fillWeaker gap-2 justify-center items-center rounded-xl cursor-pointer py-3 group-has-[#sidenav-toggle:checked]/pg:px-3"
        , tabindex_ "0"
        ]
        do
          span_ [class_ "grow hidden group-has-[#sidenav-toggle:checked]/pg:block overflow-x-hidden whitespace-nowrap truncate"] $ toHtml project.title
          faSprite_ "angles-up-down" "regular" "w-4"
      div_ [tabindex_ "0", class_ "dropdown-content z-40"] $ projectsDropDown project (Sessions.getProjects $ Sessions.projects sess.persistentSession)
    nav_ [class_ "mt-5 flex flex-col gap-2.5 text-textWeak"] do
      -- FIXME: reeanable hx-boost hxBoost_ "true"
      menu project.id & mapM_ \(mTitle, mUrl, fIcon) -> do
        let isActive = maybe (pageTitle == mTitle) (== mTitle) menuItem
        let activeCls = if isActive then "bg-fillWeak  text-textStrong border border-strokeStrong" else "border-transparent!"
        a_
          [ href_ mUrl
          , term "data-tippy-placement" "right"
          , term "data-tippy-content" mTitle
          , class_ $ "group-has-[#sidenav-toggle:checked]/pg:px-4 gap-3 py-2 flex no-wrap shrink-0  justify-center group-has-[#sidenav-toggle:checked]/pg:justify-start items-center rounded-lg  border hover:border overflow-x-hidden overflow-y-hidden " <> activeCls
          ]
          do
            faSprite_ fIcon "regular" "w-4 h-4 shrink-0 "
            span_ [class_ "hidden group-has-[#sidenav-toggle:checked]/pg:block whitespace-nowrap truncate"] $ toHtml mTitle

  div_ [class_ "py-8 px-2 group-has-[#sidenav-toggle:checked]/pg:px-3 *:gap-2 *:whitespace-nowrap *:truncate flex flex-col gap-2.5 *:items-center *:overflow-x-hidden *:flex &:no-wrap"] do
    let currUser = sess.persistentSession.user.getUser
        userIdentifier =
          if currUser.firstName /= "" || currUser.lastName /= ""
            then currUser.firstName <> " " <> currUser.lastName
            else CI.original currUser.email
        emailMd5 = decodeUtf8 $ MD5.hash $ encodeUtf8 $ CI.original currUser.email
        sanitizedID = T.replace " " "+" userIdentifier
    div_ [tabindex_ "0", role_ "button", class_ ""] do
      img_
        [ class_ "inline-block w-9 h-9 p-2 rounded-full bg-fillPress"
        , term "data-tippy-placement" "right"
        , term "data-tippy-content" userIdentifier
        , src_ [text|https://www.gravatar.com/avatar/${emailMd5}?d=https%3A%2F%2Fui-avatars.com%2Fapi%2F/${sanitizedID}/128|]
        ]
      span_ [class_ "hidden group-has-[#sidenav-toggle:checked]/pg:inline-block overflow-hidden"] $ toHtml userIdentifier

    a_
      [ class_ "hover:bg-fillBrand-weak "
      , term "data-tippy-placement" "right"
      , term "data-tippy-content" "Settings"
      , href_ $ "/p/" <> project.id.toText <> "/settings"
      ]
      $ span_ [class_ "w-9 h-9 p-2 flex justify-center items-center rounded-full bg-fillBrand-weak text-textBrand leading-none "] (faSprite_ "gear" "regular" "h-3 w-3")
      >> span_ [class_ "hidden group-has-[#sidenav-toggle:checked]/pg:block"] "Settings"
    a_
      [ class_ "hover:bg-fillBrand-weak "
      , target_ "blank"
      , term "data-tippy-placement" "right"
      , term "data-tippy-content" "Documentation"
      , href_ "https://apitoolkit.io/docs/"
      ]
      $ span_ [class_ "w-9 h-9 p-2 flex justify-center items-center rounded-full bg-fillBrand-weak text-textBrand leading-none"] (faSprite_ "circle-question" "regular" "h-3 w-3")
      >> span_ [class_ "hidden group-has-[#sidenav-toggle:checked]/pg:block"] "Documentation"

    -- Dark mode toggle
    div_
      [ class_ "hover:bg-fillBrand-weak px-2 py-1 rounded-lg"
      , term "data-tippy-placement" "right"
      , term "data-tippy-content" "Toggle dark mode"
      ]
      $ do
        -- Regular toggle with icons (visible when sidebar is expanded)
        label_ [class_ "hidden group-has-[#sidenav-toggle:checked]/pg:flex cursor-pointer gap-2 items-center justify-center"] $ do
          faSprite_ "sun-bright" "regular" "h-5 w-5 text-textBrand"
          input_
            [ type_ "checkbox"
            , class_ "toggle theme-controller"
            , id_ "dark-mode-toggle"
            , onclick_ "toggleDarkMode()"
            ]
          faSprite_ "moon-stars" "regular" "h-5 w-5 text-textBrand"

        -- Swap rotate icon (visible when sidebar is collapsed)
        label_ [class_ "swap swap-rotate group-has-[#sidenav-toggle:checked]/pg:hidden"] $ do
          input_
            [ type_ "checkbox"
            , class_ "theme-controller"
            , id_ "dark-mode-toggle-swap"
            , onclick_ "toggleDarkMode()"
            ]
          -- Sun icon (shown in light mode)
          span_ [class_ "swap-off"] $ faSprite_ "sun-bright" "regular" "h-6 w-6"
          -- Moon icon (shown in dark mode)
          span_ [class_ "swap-on"] $ faSprite_ "moon-stars" "regular" "h-6 w-6"
    a_
      [ class_ "hover:bg-fillBrand-weak"
      , term "data-tippy-placement" "right"
      , term "data-tippy-content" "Logout"
      , href_ "/logout"
      , [__| on click js posthog.reset(); end |]
      ]
      $ span_ [class_ "w-9 h-9 p-2 flex justify-center items-center  rounded-full bg-fillError-weak text-textError leading-none"] (faSprite_ "arrow-right-from-bracket" "regular" "h-3 w-3")
      >> span_ [class_ "hidden group-has-[#sidenav-toggle:checked]/pg:block"] "Logout"


-- mapM_ renderNavBottomItem $ navBottomList project.id.toText

navbar :: Maybe Projects.Project -> [(Text, Text, Text)] -> Users.User -> Maybe Text -> Text -> Maybe Text -> Maybe Text -> Maybe (Html ()) -> Maybe (Html ()) -> Html ()
navbar projectM menuL currUser prePageTitle pageTitle pageTitleMonadId docsLink tabsM pageActionsM =
  nav_ [id_ "main-navbar", class_ "w-full px-3 py-2 flex flex-row border-strokeWeak"] do
    div_ [class_ "flex-1 flex items-center text-textStrong gap-1"] do
      whenJust prePageTitle \pt -> whenJust (find (\a -> fst3 a == pt) menuL) \(_, _, icon) -> do
        whenJust projectM \p -> a_ [class_ "p-1 hover:bg-fillWeak inline-flex items-center justify-center gap-1 rounded-md text-sm", href_ $ "/p/" <> p.id.toText <> "/dashboards"] do
          faSprite_ icon "regular" "w-4 h-4 text-strokeStrong"
          toHtml pt
        faSprite_ "chevron-right" "regular" "w-3 h-3"
      label_ [class_ "font-normal text-xl p-1 rounded-md cursor-pointer hover:bg-fillWeak leading-none", Lucid.for_ $ maybeToMonoid pageTitleMonadId, id_ "pageTitleText"] $ toHtml pageTitle
      whenJust docsLink \link -> a_ [class_ "text-iconBrand -mt-1", href_ link, term "data-tippy-placement" "right", term "data-tippy-content" "Open Documentation"] $ faSprite_ "circle-question" "regular" "w-4 h-4"
    whenJust tabsM id
    div_ [class_ "flex-1 flex items-center justify-end text-sm"] $ whenJust pageActionsM id


alerts_ :: Html ()
alerts_ = do
  template_ [id_ "successToastTmpl"] do
    div_ [role_ "alert", class_ "alert alert-success w-96 cursor-pointer", [__|init wait for click or 30s then transition my opacity to 0 then remove me|]] do
      faSprite_ "circle-info" "solid" "stroke-current shrink-0 w-6 h-6"
      span_ [class_ "title"] "Something succeeded"
  template_ [id_ "errorToastTmpl"] do
    div_ [role_ "alert", class_ "alert alert-error w-96 cursor-pointer", [__|init wait for click or 30s then transition my opacity to 0 then remove me|]] do
      faSprite_ "circle-info" "solid" "stroke-current shrink-0 w-6 h-6"
      span_ [class_ "title"] "Something failed"
  section_ [class_ "fixed top-0 right-0 z-50 pt-14 pr-5 space-y-3", id_ "toastsParent"] ""
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
  div_ [class_ "flex items-center justify-between border-b border-strokeWeak bg-fillWeak px-4 py-1.5 gap-3 text-sm"] do
    div_ [class_ "flex items-center gap-2"] do
      faSprite_ "flask" "regular" "h-4 w-4 text-textBrand"
      span_ [class_ "font-medium text-textStrong"] "Demo Project"
      span_ [class_ "hidden sm:inline text-textWeak"] "· Explore APIToolkit's features"
    div_ [class_ "flex items-center gap-3"] do
      a_ [class_ "text-textBrand hover:underline underline-offset-2", href_ "https://apitoolkit.io/docs/onboarding/"] "Docs"
      a_ [class_ "py-1 px-2.5 rounded-lg bg-fillWeak hover:bg-fillHover text-textStrong border border-strokeWeak text-xs font-medium", href_ "https://calendar.app.google/1a4HG5GZYv1sjjZG6"] "Book Demo"
      a_ [class_ "py-1 px-2.5 rounded-lg bg-fillBrand-strong hover:opacity-90 text-textInverse-strong text-xs font-medium", href_ "/login"] "Start Free Trial"


settingsWrapper :: Projects.ProjectId -> Text -> Html () -> Html ()
settingsWrapper pid current pageHtml = do
  section_ [class_ "flex h-full w-full"] do
    nav_ [class_ "w-[300px]  h-full p-4 pt-8 border-r border-r-strokWeak"] do
      h1_ [class_ "text-3xl pl-5 font-medium"] $ "Settings"
      ul_ [class_ "flex flex-col mt-14 gap-2 w-full"] $ mapM_ (renderNavBottomItem current) $ navBottomList pid.toText
    main_ [class_ "w-full h-full overflow-y-auto"] do
      pageHtml


navBottomList :: Text -> [(Text, Text, Text, Text, Text, Maybe Text, Maybe Text, Maybe Text)]
navBottomList pidTxt =
  [ ("gear", "bg-fillBrand-weak", "text-textBrand", "Project settings", "/p/" <> pidTxt <> "/settings", Nothing, Nothing, Nothing)
  , ("key", "bg-fillSuccess-weak", "text-textSuccess", "API keys", "/p/" <> pidTxt <> "/apis", Nothing, Nothing, Nothing)
  , ("user-plus", "bg-fillWarning-weak", "text-textWarning", "Manage members", "/p/" <> pidTxt <> "/manage_members", Nothing, Nothing, Nothing)
  , ("dollar", "bg-fillWarning-weak", "text-textWarning", "Manage billing", "/p/" <> pidTxt <> "/manage_billing", Nothing, Nothing, Nothing)
  , ("arrows-turn-right", "bg-fillBrand-weak", "text-textBrand", "Integrations", "/p/" <> pidTxt <> "/integrations", Nothing, Nothing, Nothing)
  , ("bucket", "", "", "Your S3 bucket", "/p/" <> pidTxt <> "/byob_s3", Nothing, Nothing, Nothing)
  , ("trash", "bg-fillError-weak", "text-textError", "Delete project", "/p/" <> pidTxt <> "/settings/delete", Nothing, Nothing, Nothing)
  ]


renderNavBottomItem :: Text -> (Text, Text, Text, Text, Text, Maybe Text, Maybe Text, Maybe Text) -> Html ()
renderNavBottomItem curr (iconName, bgColor, textColor, linkText, link, targetBlankM, onClickM, hxGetM) =
  let
    defaultAttrs =
      [ class_ "hover:bg-fillBrand-weak flex gap-2 items-center "
      , term "data-tippy-placement" "right"
      , term "data-tippy-content" linkText
      ]
    activeCls = if curr == linkText then "bg-fillWeak" else ""
    attrs =
      defaultAttrs
        ++ [target_ "BLANK_" | isJust targetBlankM]
        ++ maybe [] (\onClick -> [onclick_ onClick]) onClickM
        ++ (if isJust hxGetM then [hxGet_ link, hxTarget_ "body"] else [href_ link])
   in
    li_ [class_ $ "px-2 py-1 w-[220px] rounded-lg " <> activeCls] do
      a_ attrs $ do
        span_
          [class_ "p-2 rounded-full shrink-0 leading-none"]
          (faSprite_ iconName "regular" "shrink-0 h-3 w-3")
        span_
          [class_ "text-textWeak"]
          (toHtml linkText)
