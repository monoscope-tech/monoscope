module Pages.BodyWrapper (bodyWrapper, BWConfig (..), PageCtx (..)) where

import Crypto.Hash.MD5 qualified as MD5
import Data.CaseInsensitive qualified as CI
import Data.Default (Default)
import Data.Text qualified as T
import Data.Vector qualified as Vector
import Lucid
import Lucid.Htmx (hxGet_)
import Lucid.Hyperscript (__)
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import Models.Users.Users qualified as Users
import NeatInterpolation (text)
import Pkg.Components.ExternalHeadScripts (externalHeadScripts_)
import Pkg.THUtils
import PyF
import Relude
import Utils (faSprite_)


menu :: Projects.ProjectId -> [(Text, Text, Text)]
menu pid =
  [ ("Dashboard", "/p/" <> pid.toText <> "/", "qrcode")
  , ("Explorer", "/p/" <> pid.toText <> "/log_explorer", "explore")
  , ("API Catalog", "/p/" <> pid.toText <> "/api_catalog", "swap")
  , ("Changes & Errors", "/p/" <> pid.toText <> "/anomalies", "bug")
  , ("Monitors & Alerts", "/p/" <> pid.toText <> "/monitors", "list-check")
  , ("Reports", "/p/" <> pid.toText <> "/reports", "chart-simple")
  ]


type role PageCtx representational


data PageCtx a = PageCtx
  { conf :: BWConfig
  , content :: a
  }
  deriving stock (Show, Generic)


instance ToHtml a => ToHtml (PageCtx a) where
  {-# INLINE toHtml #-}
  toHtml (PageCtx bwcfg child) = toHtmlRaw $ bodyWrapper bwcfg (toHtml child)
  {-# INLINE toHtmlRaw #-}
  toHtmlRaw (PageCtx bwcfg child) = toHtmlRaw $ bodyWrapper bwcfg (toHtmlRaw child)


-- TODO: Rename to pageCtx
data BWConfig = BWConfig
  { sessM :: Maybe Sessions.PersistentSession
  , currProject :: Maybe Projects.Project
  , pageTitle :: Text
  , menuItem :: Maybe Text -- Use PageTitle if menuItem is not set
  , hasIntegrated :: Maybe Bool
  , navTabs :: Maybe (Html ())
  , pageActions :: Maybe (Html ())
  }
  deriving stock (Show, Generic)
  deriving anyclass (Default)


bodyWrapper :: BWConfig -> Html () -> Html ()
bodyWrapper BWConfig{sessM, currProject, pageTitle, menuItem, hasIntegrated, navTabs, pageActions} child = do
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
      link_ [rel_ "stylesheet", type_ "text/css", href_ $(hashAssetFile "/public/assets/css/tailwind.min.css")]
      link_ [rel_ "stylesheet", type_ "text/css", href_ $(hashAssetFile "/public/assets/css/thirdparty/notyf3.min.css")]
      link_ [rel_ "preconnect", href_ "https://rsms.me/"]
      link_ [rel_ "stylesheet", href_ "https://rsms.me/inter/inter.css"]
      link_ [rel_ "stylesheet", href_ "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.8.0/styles/atom-one-dark.min.css"]
      link_ [rel_ "stylesheet", href_ "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.63.0/codemirror.min.css"]
      link_ [rel_ "stylesheet", href_ "https://cdnjs.cloudflare.com/ajax/libs/codemirror/6.65.7/theme/elegant.min.css"]
      link_ [href_ "https://cdn.jsdelivr.net/npm/@yaireo/tagify/dist/tagify.css", rel_ "stylesheet", type_ "text/css"]

      -- SCRIPTS
      script_ [src_ "https://cdn.jsdelivr.net/npm/@yaireo/tagify"] ("" :: Text)
      script_ [src_ "https://cdn.jsdelivr.net/npm/@yaireo/tagify/dist/tagify.polyfills.min.js"] ("" :: Text)
      script_ [src_ "https://cdn.jsdelivr.net/npm/echarts@5.4.1/dist/echarts.min.js"] ("" :: Text)
      script_ [src_ $(hashAssetFile "/public/assets/roma-echarts.js"), defer_ "true"] ("" :: Text)
      script_ [src_ $(hashAssetFile "/public/assets/js/thirdparty/notyf3.min.js"), defer_ "true"] ("" :: Text)
      script_ [src_ $(hashAssetFile "/public/assets/js/thirdparty/htmx1_9_10.min.js"), defer_ "true"] ("" :: Text)
      script_ [src_ $(hashAssetFile "/public/assets/deps/htmx/multi-swap.js"), defer_ "true"] ("" :: Text)
      script_ [src_ $(hashAssetFile "/public/assets/deps/htmx/preload.js"), defer_ "true"] ("" :: Text)
      script_ [src_ $(hashAssetFile "/public/assets/deps/htmx/json-enc.js"), defer_ "true"] ("" :: Text)
      script_ [src_ $(hashAssetFile "/public/assets/deps/lit/lit-html.js"), type_ "module", defer_ "true"] ("" :: Text)
      script_ [src_ "https://unpkg.com/htmx.org/dist/ext/debug.js", defer_ "true"] ("" :: Text)
      script_ [src_ $(hashAssetFile "/public/assets/js/thirdparty/_hyperscript_web0_9_5.min.js"), defer_ "true"] ("" :: Text)
      script_ [src_ $(hashAssetFile "/public/assets/js/thirdparty/_hyperscript_template.js"), defer_ "true"] ("" :: Text)
      script_ [src_ $(hashAssetFile "/public/assets/js/thirdparty/luxon.min.js"), defer_ "true"] ("" :: Text)
      script_ [src_ $(hashAssetFile "/public/assets/js/thirdparty/popper2_11_4.min.js"), defer_ "true"] ("" :: Text)
      script_ [src_ $(hashAssetFile "/public/assets/js/thirdparty/tippy6_3_7.umd.min.js"), defer_ "true"] ("" :: Text)
      script_ [src_ $(hashAssetFile "/public/assets/js/thirdparty/instantpage5_1_0.js"), type_ "module", defer_ "true"] ("" :: Text)
      script_ [src_ $(hashAssetFile "/public/assets/js/monaco/vs/loader.js"), defer_ "true"] ("" :: Text)
      script_ [src_ $(hashAssetFile "/public/assets/js/charts.js")] ("" :: Text)
      script_ [src_ "https://cdn.jsdelivr.net/npm/@easepick/bundle@1.2.0/dist/index.umd.min.js"] ("" :: Text)
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
      script_ [type_ "module", src_ $(hashAssetFile "/public/assets/filtercomponent.js")] ("" :: Text)
      script_ [src_ "/public/assets/js/main.js"] ("" :: Text)

      script_
        [text|
      !function(e,t,n,s,u,a){e.twq||(s=e.twq=function(){s.exe?s.exe.apply(s,arguments):s.queue.push(arguments);},s.version='1.1',s.queue=[],u=t.createElement(n),u.async=!0,u.src='https://static.ads-twitter.com/uwt.js',
      a=t.getElementsByTagName(n)[0],a.parentNode.insertBefore(u,a))}(window,document,'script');
      twq('config','om5gt');
      |]

      script_
        [raw|
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
                if (c.startsWith(name)) {
                    return c.substring(name.length);
                }
            }
            return "";
        }

        var currentISOTimeStringVar = ((new Date()).toISOString().split(".")[0])+"+00:00";
        document.addEventListener('DOMContentLoaded', function(){
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
              navigator.serviceWorker.register("/public/sw.js?v=7").then(swReg => {}).catch(err => {
                  console.error('Service Worker Error', err);
              });
          });
        }
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
                _hyperscript.processNode(.log-item-context-menu) then htmx.process(next <.log-item-context-menu/>)
              end
            end
          end
    |]

    body_ [class_ "h-full w-full bg-slate-50 text-slate-500 group/pg", term "data-theme" "antdtheme", term "hx-ext" "multi-swap,preload"] do
      div_
        [ style_ "z-index:99999"
        , class_ "pt-24 sm:hidden justify-center z-50 w-full p-4 bg-gray-50 overflow-y-auto inset-0 h-full max-h-full"
        , tabindex_ "-1"
        ]
        do
          div_ [class_ "relative mx-auto max-h-full", style_ "width: min(90vw, 500px)"] $
            div_ [class_ "bg-base-100 rounded-lg drop-shadow-md border-1 w-full"] do
              div_ [class_ "flex items-start justify-between p-6 space-x-2  border-b rounded-t"] do
                h3_ [class_ "text-3xl font-bold "] "Only Desktop Browsers are Supported for now!"
              -- Modal body
              div_ [class_ "w-full"] $ div_ [class_ "p-6 space-y-6", style_ "height:50vh; width:100%"] do
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
                section_ [class_ "h-screen overflow-y-hidden grow"] do
                  navbar currUser pageTitle navTabs pageActions
                  section_ [class_ "overflow-y-hidden h-full "] child
      externalHeadScripts_
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

          document.body.addEventListener('htmx:afterSwap', function (event) {
            window.requestAnimationFrame(() => {
              hljs.highlightAll();
            });
          });
      |]
      let email = show $ maybe "" ((.user.getUser.email)) sessM
      let name = maybe "" (\sess -> sess.user.getUser.firstName <> " " <> sess.user.getUser.lastName) sessM
      script_
        [text| window.addEventListener("load", (event) => {
        posthog.people.set_once({email: ${email}, name: "${name}"});
      });|]


projectsDropDown :: Projects.Project -> Vector.Vector Projects.Project -> Html ()
projectsDropDown currProject projects = do
  let pidTxt = currProject.id.toText
  div_
    [ term "data-menu" "true"
    , class_ "origin-top-right z-40 transition transform bg-base-100 p-4 absolute w-[20rem] rounded-2xl shadow-2xl shadow-indigo-200 opacity-100 scale-100"
    ]
    do
      div_ [class_ "p-2 pb-4 "] do
        div_ [class_ "flex mt-2 mb-4"] do
          faSprite_ "folders" "regular" "h-5 w-5 mr-2"
          div_ do
            strong_ [class_ "block"] $ toHtml currProject.title
            small_ [class_ "block"] $ toHtml currProject.paymentPlan
        nav_ [] do
          a_ [href_ [text| /p/$pidTxt/settings |], class_ "p-3 flex gap-3 items-center rounded-2xl hover:bg-gray-100"] do
            faSprite_ "gear" "regular" "h-5 w-5" >> span_ "Settings"
          a_ [href_ [text| /p/$pidTxt/manage_members |], class_ "p-3 flex gap-3 items-center rounded hover:bg-gray-100"] do
            faSprite_ "user-plus" "regular" "h-5 w-5" >> span_ "Manage members"
          a_ [href_ [text| /p/$pidTxt/apis|], class_ "p-3 flex gap-3 items-center rounded hover:bg-gray-100"] do
            faSprite_ "key" "regular" "h-5 w-5" >> span_ "API Keys"
          a_ [href_ [text| /p/$pidTxt/integrations|], class_ "p-3 flex gap-3 items-center rounded hover:bg-gray-100"] do
            faSprite_ "arrows-turn-right" "regular" "h-5 w-5" >> span_ "Integrations"
          when (currProject.paymentPlan == "UsageBased" || currProject.paymentPlan == "GraduatedPricing") $
            a_
              [class_ "p-3 flex gap-3 items-center rounded hover:bg-gray-100 cursor-pointer", hxGet_ [text| /p/$pidTxt/manage_subscription |]]
              (faSprite_ "dollar-sign" "regular" "h-5 w-5" >> span_ "Manage billing")
      div_ [class_ "border-t border-gray-100 p-2"] do
        div_ [class_ "flex justify-between content-center items-center py-5 mb-2 "] do
          a_ [href_ "/"] $ h3_ [] "Switch projects"
          a_ [class_ "bg-blue-700 flex pl-3 pr-4 py-2 rounded-xl text-white space-x-2", href_ "/p/new"] do
            faSprite_ "plus" "regular" "h-5 w-5 bg-blue-800 rounded-lg" >> span_ [class_ "inline-block px-1"] "Add"
        div_ do
          div_ [class_ "relative"] do
            div_ [class_ "absolute inset-y-0 left-0 pl-4 flex items-center pointer-events-none"] $ faSprite_ "magnifying-glass" "regular" "h-6 w-4"
            input_
              [ class_ "pl-12 w-full  bg-gray-100 rounded-2xl border-0 p-3"
              , placeholder_ "Search Projects"
              , [__|on input show .project_item in #projectsContainer when its textContent.toLowerCase() contains my value.toLowerCase()|]
              ]
          div_ [class_ "space-y-2 py-4 ", id_ "projectsContainer"] do
            projects & mapM_ \project -> do
              a_ [class_ "flex justify-between p-2 project_item", href_ $ "/p/" <> project.id.toText] do
                div_ [class_ "space-x-3"] $
                  faSprite_ "folders" "regular" "h-5 w-5 inline-block"
                    >> span_ [class_ "inline-block"] (toHtml project.title)
                when (currProject.id == project.id) $ faSprite_ "circle-check" "regular" "h-6 w-6 text-green-700"


sideNav :: Sessions.PersistentSession -> Projects.Project -> Text -> Maybe Text -> Maybe Bool -> Html ()
sideNav sess project pageTitle menuItem hasIntegrated = aside_ [class_ "border-r bg-slate-100 border-slate-200 w-15 group-has-[#sidenav-toggle:checked]/pg:w-68  h-screen transition-all duration-200 ease-in-out flex flex-col justify-between", id_ "side-nav-menu"] do
  div_ [class_ "px-2 group-has-[#sidenav-toggle:checked]/pg:px-6"] do
    div_ [class_ "py-4 flex justify-center group-has-[#sidenav-toggle:checked]/pg:justify-between items-center"] do
      a_ [href_ "/", class_ "inline-flex"] do
        img_ [class_ "h-5 hidden group-has-[#sidenav-toggle:checked]/pg:block", src_ "/public/assets/svgs/logo.svg"]
        img_ [class_ "h-10 w-10 hidden sd-show", src_ "/public/assets/logo-mini.png"]
      label_ [class_ "cursor-pointer text-slate-500"] do
        input_ [type_ "checkbox", class_ "hidden", id_ "sidenav-toggle", [__|on change call setCookie("isSidebarClosed", `${me.checked}`)|]]
        script_ [text|document.getElementById("sidenav-toggle").checked= getCookie("isSidebarClosed")=="true" |]
        faSprite_ "side-chevron-left-in-box" "regular" " h-5 w-5 opacity-60 rotate-180 group-has-[#sidenav-toggle:checked]/pg:rotate-0"
    div_ [class_ "mt-4 sd-px-0 dropdown block"] do
      a_
        [ class_ "flex flex-row border border-slate-300 bg-slate-50 text-slate-950 hover:bg-slate-100 gap-2 justify-center rounded-lg cursor-pointer py-2 group-has-[#sidenav-toggle:checked]/pg:px-3"
        , tabindex_ "0"
        ]
        do
          span_ [class_ "grow hidden group-has-[#sidenav-toggle:checked]/pg:block overflow-x-hidden whitespace-nowrap truncate"] $ toHtml project.title
          faSprite_ "angles-up-down" "regular" " w-4 m-1"
      div_ [tabindex_ "0", class_ "dropdown-content z-[40]"] $ projectsDropDown project (Sessions.getProjects $ Sessions.projects sess)
    nav_ [class_ "mt-5 flex flex-col gap-2.5"] do
      -- FIXME: reeanable hx-boost hxBoost_ "true"
      menu project.id & mapM_ \(mTitle, mUrl, fIcon) -> do
        let isActive = maybe (pageTitle == mTitle) (== mTitle) menuItem
        let activeCls = if isActive then " bg-slate-50 text-slate-950 border shadow " else " border-transparent "
        a_
          [ href_ mUrl
          , term "data-tippy-placement" "right"
          , term "data-tippy-content" mTitle
          , class_ $ "group-has-[#sidenav-toggle:checked]/pg:px-4 gap-3 py-2 flex no-wrap shrink-0  justify-center group-has-[#sidenav-toggle:checked]/pg:justify-start items-center rounded-lg border-slate-300 hover:shadow hover:border  overflow-x-hidden h-9 overflow-y-hidden " <> activeCls
          ]
          do
            faSprite_ fIcon "regular" "w-4 h-4 shrink-0 "
            span_ [class_ "hidden group-has-[#sidenav-toggle:checked]/pg:block whitespace-nowrap truncate"] $ toHtml mTitle

  div_ [class_ "py-8 px-2 group-has-[#sidenav-toggle:checked]/pg:px-6 [&>*]:gap-2 [&>*]:whitespace-nowrap [&>*]:truncate flex flex-col gap-2.5 [&>*]:items-center [&>*]:overflow-x-hidden [&>*]:flex &:no-wrap"] do
    let currUser = sess.user.getUser
    let userIdentifier =
          if currUser.firstName /= "" || currUser.lastName /= ""
            then currUser.firstName <> " " <> currUser.lastName
            else CI.original currUser.email
    let emailMd5 = decodeUtf8 $ MD5.hash $ encodeUtf8 $ CI.original currUser.email
    let sanitizedID = T.replace " " "+" userIdentifier
    div_ [tabindex_ "0", role_ "button", class_ "cursor-pointer justify-center group-has-[#sidenav-toggle:checked]/pg:justify-start"] do
      img_
        [ class_ "inline-block w-10 h-10 p-2 rounded-full bg-gray-300"
        , term "data-tippy-placement" "right"
        , term "data-tippy-content" userIdentifier
        , src_ $ [text|https://www.gravatar.com/avatar/${emailMd5}?d=https%3A%2F%2Fui-avatars.com%2Fapi%2F/${sanitizedID}/128|]
        ]
      span_ [class_ "hidden group-has-[#sidenav-toggle:checked]/pg:inline-block overflow-hidden"] $ toHtml userIdentifier
    a_
      [ class_ "hover:bg-blue-50 "
      , target_ "blank"
      , term "data-tippy-placement" "right"
      , term "data-tippy-content" "Documentation"
      , href_ "https://apitoolkit.io/docs/"
      ]
      do
        span_ [class_ "p-3 rounded-full bg-blue-100 text-blue-500 leading-none"] (faSprite_ "circle-question" "regular" "h-4 w-4") >> span_ [class_ "hidden group-has-[#sidenav-toggle:checked]/pg:block"] "Documentation"
    a_
      [ class_ "hover:bg-blue-50"
      , term "data-tippy-placement" "right"
      , term "data-tippy-content" "Logout"
      , href_ "/logout"
      , [__| on click js posthog.reset(); end |]
      ]
      do
        span_ [class_ "p-3 rounded-full bg-red-100 text-red-600 leading-none"] (faSprite_ "arrow-right-from-bracket" "regular" "h-4 w-4") >> span_ [class_ "hidden group-has-[#sidenav-toggle:checked]/pg:block"] "Logout"


navbar :: Users.User -> Text -> Maybe (Html ()) -> Maybe (Html ()) -> Html ()
navbar currUser pageTitle tabsM pageActionsM =
  nav_ [id_ "main-navbar", class_ "sticky z-20 top-0 w-full px-6 py-1 flex flex-row border-b border-slate-200 h-[3.2rem]"] do
    div_ [class_ "flex-1 flex items-center"] $ toHtml pageTitle
    whenJust tabsM id
    div_ [class_ "flex-1 flex items-center justify-end"] $ whenJust pageActionsM id


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
