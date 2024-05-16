module Pages.BodyWrapper (bodyWrapper, BWConfig (..)) where

import Data.CaseInsensitive qualified as CI
import Data.Default (Default)
import Data.Vector qualified as Vector
import Lucid
import Lucid.Htmx (hxGet_)
import Lucid.Hyperscript (__)
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import Models.Users.Users qualified as Users
import NeatInterpolation (text)
import Pkg.Components.ExternalHeadScripts (externalHeadScripts_)
import PyF
import Relude
import Utils (faSprite_)


menu :: Projects.ProjectId -> [(Text, Text, Text)]
menu pid =
  [ ("Get Started", "/p/" <> pid.toText <> "/onboarding", "list-check")
  , ("Dashboard", "/p/" <> pid.toText <> "/", "qrcode")
  , ("Endpoints", "/p/" <> pid.toText <> "/endpoints", "swap")
  , ("Outbound Integrations", "/p/" <> pid.toText <> "/outgoing", "arrows-turn-right")
  , ("Changes & Errors", "/p/" <> pid.toText <> "/anomalies?ackd=false&archived=false", "bug")
  , ("API Log Explorer", "/p/" <> pid.toText <> "/log_explorer", "list-tree")
  , -- , ("Redacted Fields", "/p/" <> pid.toText <> "/redacted_fields", "#redacted")
    ("Documentation", "/p/" <> pid.toText <> "/documentation", "brackets-curly")
  , ("Reports", "/p/" <> pid.toText <> "/reports", "chart-simple")
  ]


-- TODO: Rename to pageCtx
data BWConfig = BWConfig
  { sessM :: Maybe Sessions.PersistentSession
  , currProject :: Maybe Projects.Project
  , pageTitle :: Text
  , menuItem :: Maybe Text -- Use PageTitle if menuItem is not set
  , hasIntegrated :: Maybe Bool
  }
  deriving stock (Show, Generic)
  deriving anyclass (Default)


bodyWrapper :: BWConfig -> Html () -> Html ()
bodyWrapper BWConfig{sessM, currProject, pageTitle, menuItem, hasIntegrated} child = do
  doctypehtml_ do
    head_ do
      title_ $ toHtml pageTitle
      meta_ [charset_ "UTF-8"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
      meta_ [httpEquiv_ "X-UA-Compatible", content_ "ie=edge"]
      -- favicon items
      link_ [rel_ "apple-touch-icon", sizes_ "180x180", href_ "/public/apple-touch-icon.png"]
      link_ [rel_ "icon", type_ "image/png", sizes_ "32x32", href_ "/public/favicon-32x32.png"]
      link_ [rel_ "icon", type_ "image/png", sizes_ "16x16", href_ "/public/favicon-16x16.png"]
      link_ [rel_ "manifest", href_ "/public/site.webmanifest"]
      link_ [rel_ "mask-icon", href_ "/public/safari-pinned-tab.svg", term "color" "#5bbad5"]
      meta_ [name_ "msapplication-TileColor", content_ "#da532c"]
      meta_ [name_ "theme-color", content_ "#ffffff"]
      link_ [rel_ "stylesheet", type_ "text/css", href_ "/assets/css/tailwind.min.css?v=2"]
      link_ [rel_ "stylesheet", type_ "text/css", href_ "/assets/css/thirdparty/notyf3.min.css"]
      link_ [rel_ "preconnect", href_ "https://rsms.me/"]
      link_ [rel_ "stylesheet", href_ "https://rsms.me/inter/inter.css"]
      link_ [rel_ "stylesheet", href_ "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.8.0/styles/atom-one-dark.min.css"]
      link_ [rel_ "stylesheet", href_ "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.63.0/codemirror.min.css"]
      link_ [rel_ "stylesheet", href_ "https://cdnjs.cloudflare.com/ajax/libs/codemirror/6.65.7/theme/elegant.min.css"]

      -- SCRIPTS
      script_ [src_ "https://cdn.jsdelivr.net/npm/echarts@5.4.1/dist/echarts.min.js"] ("" :: Text)
      script_ [src_ "/assets/roma-echarts.js", defer_ "true"] ("" :: Text)
      script_ [src_ "/assets/js/thirdparty/notyf3.min.js", defer_ "true"] ("" :: Text)
      script_ [src_ "/assets/js/thirdparty/htmx1_9_10.min.js", defer_ "true"] ("" :: Text)
      script_ [src_ "/assets/deps/htmx/multi-swap.js", defer_ "true"] ("" :: Text)
      script_ [src_ "/assets/deps/htmx/preload.js", defer_ "true"] ("" :: Text)
      script_ [src_ "/assets/deps/htmx/json-enc.js", defer_ "true"] ("" :: Text)
      script_ [src_ "/assets/deps/lit/lit-html.js", type_ "module", defer_ "true"] ("" :: Text)
      script_ [src_ "https://unpkg.com/htmx.org/dist/ext/debug.js", defer_ "true"] ("" :: Text)
      script_ [src_ "/assets/js/thirdparty/_hyperscript_web0_9_5.min.js", defer_ "true"] ("" :: Text)
      script_ [src_ "/assets/js/thirdparty/_hyperscript_template.js", defer_ "true"] ("" :: Text)
      script_ [src_ "/assets/js/thirdparty/luxon.min.js", defer_ "true"] ("" :: Text)
      script_ [src_ "/assets/js/thirdparty/popper2_11_4.min.js", defer_ "true"] ("" :: Text)
      script_ [src_ "/assets/js/thirdparty/tippy6_3_7.umd.min.js", defer_ "true"] ("" :: Text)
      script_ [src_ "/assets/js/thirdparty/instantpage5_1_0.js", type_ "module", defer_ "true"] ("" :: Text)
      script_ [src_ "/assets/js/monaco/vs/loader.js", defer_ "true"] ("" :: Text)
      script_ [src_ "/assets/js/charts.js"] ("" :: Text)
      script_ [src_ "/assets/js/main.js"] ("" :: Text)
      script_ [src_ "https://cdn.jsdelivr.net/npm/lodash@4.17.21/lodash.min.js"] ("" :: Text)
      script_ [src_ "https://kit.fontawesome.com/e0cb5637ed.js", crossorigin_ "anonymous"] ("" :: Text)
      script_ [src_ "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.8.0/highlight.min.js"] ("" :: Text)
      script_ [src_ "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.8.0/languages/go.min.js"] ("" :: Text)
      script_ [src_ "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.8.0/languages/javascript.min.js"] ("" :: Text)
      script_ [src_ "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.8.0/languages/php.min.js"] ("" :: Text)
      script_ [src_ "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.8.0/languages/csharp.min.js"] ("" :: Text)
      script_ [src_ "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.8.0/languages/python.min.js"] ("" :: Text)
      script_ [src_ "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.63.0/codemirror.min.js"] ("" :: Text)
      script_ [src_ "https://cdnjs.cloudflare.com/ajax/libs/codemirror/6.65.7/mode/javascript/javascript.min.js"] ("" :: Text)
      script_ [type_ "module", src_ "/assets/filtercomponent.js"] ("" :: Text)

      -- script_ [src_ "https://cdn.jsdelivr.net/npm/@easepick/core@1.2.0/dist/index.umd.min.js"] ("" :: Text)
      -- script_ [src_ "https://cdn.jsdelivr.net/npm/@easepick/datetime@1.2.0/dist/index.umd.min.js"] ("" :: Text)
      -- script_ [src_ "https://cdn.jsdelivr.net/npm/@easepick/base-plugin@1.2.0/dist/index.umd.min.js"] ("" :: Text)
      -- script_ [src_ "https://cdn.jsdelivr.net/npm/@easepick/range-plugin@1.2.0/dist/index.umd.min.js"] ("" :: Text)
      -- script_ [src_ "https://cdn.jsdelivr.net/npm/@easepick/preset-plugin@1.2.0/dist/index.umd.min.js"] ("" :: Text)
      -- script_ [src_ "https://cdn.jsdelivr.net/npm/@easepick/time-plugin@1.2.0/dist/index.umd.min.js"] ("" :: Text)
      script_ [src_ "https://cdn.jsdelivr.net/npm/@easepick/bundle@1.2.0/dist/index.umd.min.js"] ("" :: Text)

      script_
        [raw|
              window.initialCloseSideMenu = localStorage.getItem('close-sidemenu');
              var currentISOTimeStringVar = ((new Date()).toISOString().split(".")[0])+"+00:00";
              document.addEventListener('DOMContentLoaded', function(){ 
                if (window.initialCloseSideMenu == 'true'){
                   document.getElementById('side-nav-menu').classList.add('hidden-side-nav-menu');
                }

                // htmx.config.useTemplateFragments = true
                tippy('[data-tippy-content]');
                var notyf = new Notyf({
                    duration: 5000,
                    position: {
                    x: 'right',
                    y: 'top',
                  },
                });
                document.body.addEventListener("successToast", (e)=> {e.detail.value.map(v=>notyf.success(v));});
                document.body.addEventListener("errorToast", (e)=> {e.detail.value.map(v=>notyf.error(v));});
              });


              if("serviceWorker" in navigator) {
                  window.addEventListener("load", () => {
                    navigator.serviceWorker.register("/public/sw.js?v=4").then(swReg => {}).catch(err => {
                        console.error('Service Worker Error', err);
                    });
                });
              }
            |]

    body_ [class_ "text-gray-900 h-full w-full bg-white fixed", term "data-theme" "winter", term "hx-ext" "multi-swap,preload"] do
      div_
        [ style_ "z-index:99999"
        , class_ "fixed pt-24 sm:hidden justify-center z-50 w-full p-4 bg-gray-50 overflow-y-auto inset-0 h-full max-h-full"
        , tabindex_ "-1"
        ]
        do
          div_ [class_ "relative mx-auto max-h-full", style_ "width: min(90vw, 500px)"]
            $ div_ [class_ "bg-white rounded-lg drop-shadow-md border-1 w-full"] do
              div_ [class_ "flex items-start justify-between p-6 space-x-2  border-b rounded-t"] do
                h3_ [class_ "text-3xl font-bold text-gray-900"] "Only Desktop Browsers are Supported for now!"
              -- Modal body
              div_ [class_ "w-full"] $ div_ [class_ "p-6 text-xl space-y-6", style_ "height:50vh; width:100%"] do
                p_ [class_ ""] "Due to the heavy visualization usecases we're solving, APItoolkit is not supported on mobile, and can only be used from a desktop browser at the moment."
                p_ [class_ ""] "We're diligently working on expanding its availability to other platforms, and we'll keep you updated as we make progress. "
                p_ [] "Don't hesitate to let us know if this is a very important feature for your team, then we can prioritize it"
              -- Modal footer
              div_ [class_ "flex w-full justify-end items-center p-6 space-x-2 border-t border-gray-200 rounded-b"] pass
      case sessM of
        Nothing -> do
          section_ [class_ "flex flex-col grow  h-screen overflow-y-hidden"] do
            -- navbar currUser
            section_ [class_ "flex-1 overflow-y-auto "] do
              child
        Just sess ->
          let currUser = sess.user.getUser
              sideNav' = currProject & maybe "" \project -> sideNav sess project pageTitle menuItem hasIntegrated
           in section_ [class_ "flex flex-row h-screen overflow-hidden"] do
                sideNav'
                section_ [class_ "flex flex-col grow h-screen overflow-y-hidden"] do
                  navbar currUser
                  section_ [class_ "flex-1 overflow-y-hidden h-full grow"] $ child
      externalHeadScripts_
      script_ [async_ "true", src_ "https://www.googletagmanager.com/gtag/js?id=AW-11285541899"] ("" :: Text)
      script_
        [text|
            window.dataLayer = window.dataLayer || [];
            function gtag(){dataLayer.push(arguments);}
            gtag('js', new Date());
            gtag('config', 'AW-11285541899');

            function gtag_report_conversion(url) {
              var callback = function () {
                if (typeof(url) != 'undefined') {
                  window.location = url;
                }
              };
              gtag('event', 'conversion', {
                  'send_to': 'AW-11285541899/IUBqCKOA-8sYEIvoroUq',
                  'event_callback': callback
              });
              return false;
            }


      document.body.addEventListener('htmx:afterSwap', function (event) {
        window.requestAnimationFrame(() => {
          hljs.highlightAll();
        });
      });

          |]
      script_
        [type_ "text/hyperscript"]
        [text|
          behavior LogItemMenuable
            on click
              if I match <.with-context-menu/> then
                remove <.log-item-context-menu /> then remove .with-context-menu from <.with-context-menu />
              else
                remove <.log-item-context-menu /> then remove .with-context-menu from <.with-context-menu /> then
                get #log-item-context-menu-tmpl.innerHTML then put it after me then add .with-context-menu to me then 
                _hyperscript.processNode(.log-item-context-menu)
                htmx.process(.log-item-context-menu)
              end
              halt
            end
          end

        |]


projectsDropDown :: Projects.Project -> Vector.Vector Projects.Project -> Html ()
projectsDropDown currProject projects = do
  let pidTxt = currProject.id.toText
  div_
    [ term "data-menu" "true"
    , class_ "hidden origin-top-right z-40 transition transform bg-white p-4 absolute w-[20rem] rounded-2xl shadow-2xl shadow-indigo-200"
    , [__|
          on open
              remove .hidden
              add .ease-out .duration-100 .opacity-0 .scale-95
              wait a tick toggle .opacity-0 .opacity-100 .scale-95 .scale-100
              settle remove .ease-out .duration-100
          end
          on close
              toggle .ease-in .duration-75
              wait a tick toggle .opacity-100 .opacity-0 .scale-100 .scale-95 
              settle remove .ease-in .duration-75 .opacity-0 .opacity-100 .scale-95 .scale-100
              add .hidden 
          end
          |]
    ]
    do
      div_ [class_ "p-2 pb-4 "] do
        div_ [class_ "flex mt-2 mb-4"] do
          faSprite_ "folders" "sharp-light" "h-5 w-5 mr-2"
          div_ do
            strong_ [class_ "block"] $ toHtml currProject.title
            small_ [class_ "block text-blue-800"] $ toHtml currProject.paymentPlan
        nav_ [] do
          a_ [href_ [text| /p/$pidTxt/settings |], class_ "p-3 flex gap-3 items-center rounded-2xl hover:bg-gray-100"] do
            faSprite_ "gear" "sharp-regular" "h-5 w-5" >> span_ "Settings"
          a_ [href_ [text| /p/$pidTxt/manage_members |], class_ "p-3 flex gap-3 items-center rounded hover:bg-gray-100"] do
            faSprite_ "user-plus" "regular" "h-5 w-5" >> span_ "Manage members"
          a_ [href_ [text| /p/$pidTxt/apis|], class_ "p-3 flex gap-3 items-center rounded hover:bg-gray-100"] do
            faSprite_ "key" "regular" "h-5 w-5" >> span_ "API Keys"
          when (currProject.paymentPlan == "UsageBased")
            $ a_ [class_ "p-3 flex gap-3 flex gap-3 items-center rounded hover:bg-gray-100 cursor-pointer", hxGet_ [text| /p/$pidTxt/manage_subscription |]] 
              (faSprite_ "dollar-sign" "regular" "h-5 w-5" >> span_ "Manage billing")
      div_ [class_ "border-t border-gray-100 p-2"] do
        div_ [class_ "flex justify-between content-center items-center py-5 mb-2 "] do
          a_ [href_ "/"] $ h3_ [class_ "text-xl"] "Switch projects"
          a_ [class_ "inline-block bg-blue-700 flex pl-3 pr-4 py-2 rounded-xl text-white space-x-2", href_ "/p/new"] do
            faSprite_ "plus" "sharp-regular" "h-5 w-5 bg-blue-800 rounded-lg" >> span_ [class_ "inline-block px-1"] "Add"
        div_ do
          div_ [class_ "relative"] do
            div_ [class_ "absolute inset-y-0 left-0 pl-4 flex items-center pointer-events-none"] $ faSprite_ "magnifying-glass" "regular" "h-6 w-4"
            input_
              [ class_ "pl-12 w-full text-sm bg-gray-100 rounded-2xl border-0 p-3"
              , placeholder_ "Search Projects"
              , [__|on input show .project_item in #projectsContainer when its textContent.toLowerCase() contains my value.toLowerCase()|]
              ]
          div_ [class_ "space-y-2 py-4 text-sm", id_ "projectsContainer"] do
            projects & mapM_ \project -> do
              a_ [class_ "flex justify-between p-2 project_item", href_ $ "/p/" <> project.id.toText] do
                div_ [class_ "space-x-3"]
                  $ faSprite_ "folders" "sharp-regular" "h-5 w-5 inline-block"
                  >> span_ [class_ "inline-block"] (toHtml project.title)
                when (currProject.id == project.id) $ faSprite_ "circle-check" "sharp-regular" "h-6 w-6 text-green-700"


sideNav :: Sessions.PersistentSession -> Projects.Project -> Text -> Maybe Text -> Maybe Bool -> Html ()
sideNav sess project pageTitle menuItem hasIntegrated = do
  aside_ [class_ "shrink-0 top-0 border-r bg-white border-gray-200 w-16 h-screen overflow-hidden transition-all duration-200 ease-in-out", id_ "side-nav-menu"] do
    script_
      [text|if (window.initialCloseSideMenu == 'true'){
              document.getElementById('side-nav-menu').classList.add('hidden-side-nav-menu');
              }|]

    div_ [class_ "text-center"] do
      a_ [href_ "/", class_ "inline-block px-2 py-2 flex items-center justify-center h-12"] do
        img_ [class_ "w-40 mt-2 sd-hidden", src_ "/assets/svgs/logo.svg"]
        img_ [class_ "w-10 mt-2 hidden sd-show", src_ "/assets/logo-mini.png"]
    div_ [class_ "py-4 px-4 transition-all  duration-1000 ease-in-out", id_ "side-nav-ctx-btn"] do
      a_
        [ class_ "flex flex-row bg-blue-50 hover:bg-blue-100 text-blue-900 block p-6 rounded-md cursor-pointer"
        , [__| 
                on click queue first
                    if I do not match .active
                        add .active
                        send open to <[data-menu]/> 
                    else 
                        remove .active
                        send close to <[data-menu]/> 
                    end
                end
                on keyup[key is 'Escape'] from <body/>  
                    if I match .active
                        remove .active
                        send close to <[data-menu]/> in me
                    end
                end
            |]
        ]
        do
          div_ [class_ "space-2 grow sd-hidden"] do
            strong_ [class_ "block text-slate-900"] $ toHtml project.title
            small_ [class_ "block text-slate-900"] $ toHtml project.paymentPlan
          -- Development?
          div_ [class_ "flex flex-col"] do
            faSprite_ "chevron-up" "regular" " h-4 w-4 m-1"
            faSprite_ "chevron-down" "regular" " h-4 w-4 m-1"

      projectsDropDown project (Sessions.getProjects $ Sessions.projects sess)
    nav_ [class_ "mt-4"] do
      -- FIXME: reeanable hx-boost hxBoost_ "true"
      menu project.id & mapM_ \(mTitle, mUrl, fIcon) -> do
        let isActive = maybe (pageTitle == mTitle) (== mTitle) menuItem
        let activeCls = if isActive then " bg-blue-50 text-blue-700 border-blue-700" else " border-transparent text-slate-900"
        a_
          [ href_ mUrl
          , term "data-tippy-placement" "right"
          , term "data-tippy-content" mTitle
          , class_ $ " block flex gap-3 px-5 py-3 flex no-wrap shrink-0 items-center border-l-4 hover:bg-blue-50" <> activeCls
          ]
          do
            faSprite_ fIcon "regular" $ "w-5 h-5 shrink-0" <> if isActive then "text-blue-900 " else "text-slate-500 "
            span_ [class_ "sd-hidden "] $ toHtml mTitle


navbar :: Users.User -> Html ()
navbar currUser = do
  nav_ [id_ "main-navbar", class_ "sticky z-20 top-0 w-full px-6 py-2 border-b bg-white flex flex-row justify-between"] do
    a_
      [ id_ "side_nav_toggler"
      , class_ "cursor-pointer flex items-center"
      , [__|
      on click 
        if (localStorage.getItem('close-sidemenu') != 'true') then  
          add .hidden-side-nav-menu to #side-nav-menu then 
          call localStorage.setItem('close-sidemenu', 'true')
        else remove  .hidden-side-nav-menu from #side-nav-menu then 
          call localStorage.removeItem('close-sidemenu') 
        end
        |]
      ]
      do
        faSprite_ "bars-sort" "regular" "w-5 h-5 text-gray-500"
    div_ [class_ "inline-block flex items-center"] do
      a_ [class_ "inline-block p-2 px-3 align-middle"] $ faSprite_ "magnifying-glass" "regular" "w-5 h-5 text-gray-500"
      a_ [class_ "inline-block border-r-2 p-2 pr-5"] $ faSprite_ "bell" "regular" "w-5 h-5 text-gray-500"
      a_
        [ class_ "cursor-pointer inline-block space-x-4 pl-4 relative "
        , [__| 
            on click queue first
                if I do not match .active
                    add .active
                    send open to <[drop-menu]/> 
                else 
                    remove .active
                    send close to <[drop-menu]/> 
                end
            end
            on keyup[key is 'Escape'] from <body/>  
                if I match .active
                    remove .active
                    send close to <[drop-menu]/> in me
                end
            end
        |]
        ]
        do
          img_ [class_ "inline-block w-9 h-9 rounded-lg bg-gray-300", src_ currUser.displayImageUrl]
          span_ [class_ "inline-block"]
            $ toHtml
            $ if currUser.firstName /= "" || currUser.lastName /= ""
              then currUser.firstName <> " " <> currUser.lastName
              else CI.original currUser.email
          faSprite_ "caret-down" "solid" "w-4 h-4 inline-block"

      -- logout dropdown
      div_
        [ term "drop-menu" "true"
        , class_ "hidden origin-top-left border border-gray-100 w-[10rem] rounded-lg shadow-2xl shadow-indigo-200 z-40 transition transform bg-white p-1 absolute top-14 right-5 "
        , [__|
            on open
                remove .hidden
                add .ease-out .duration-100 .opacity-0 .scale-95
                wait a tick toggle .opacity-0 .opacity-100 .scale-95 .scale-100
                settle remove .ease-out .duration-100
            end
            on close
                toggle .ease-in .duration-75
                wait a tick toggle .opacity-100 .opacity-0 .scale-100 .scale-95 
                settle remove .ease-in .duration-75 .opacity-0 .opacity-100 .scale-95 .scale-100
                add .hidden 
            end
            |]
        ]
        do
          -- dropdown mainbody
          a_ [class_ "text-base p-2 flex gap-3 rounded hover:bg-gray-100", href_ "/logout"] do
            faSprite_ "user-plus" "regular" "h-5 w-5" >> span_ "Logout"
