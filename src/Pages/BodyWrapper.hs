module Pages.BodyWrapper (bodyWrapper, BWConfig (..)) where

import Data.CaseInsensitive qualified as CI
import Data.Default (Default)
import Data.Vector qualified as Vector
import Lucid
import Lucid.Hyperscript
import Lucid.Svg (use_)
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import Models.Users.Users qualified as Users
import NeatInterpolation
import Pages.Charts.Charts qualified as Charts
import Relude

menu :: Projects.ProjectId -> [(Text, Text, Text)]
menu pid =
  [ ("Dashboard", "/p/" <> pid.toText <> "/", "#dashboard")
  , ("Endpoints", "/p/" <> pid.toText <> "/endpoints", "#endpoint")
  , ("Anomalies", "/p/" <> pid.toText <> "/anomalies?ackd=false&archived=false", "#anomalies")
  , ("API Log Explorer", "/p/" <> pid.toText <> "/log_explorer", "#logs")
  , ("API Keys", "/p/" <> pid.toText <> "/apis", "#api")
  , ("Redacted Fields", "/p/" <> pid.toText <> "/redacted_fields", "#redacted")
  , ("Documentation", "/p/" <> pid.toText <> "/documentation", "#documentation")
  ]

data BWConfig = BWConfig
  { sessM :: Maybe Sessions.PersistentSession
  , currProject :: Maybe Projects.Project
  , pageTitle :: Text
  , menuItem :: Maybe Text -- Use PageTitle if menuItem is not set
  }
  deriving stock (Show, Generic)
  deriving anyclass (Default)

bodyWrapper :: BWConfig -> Html () -> Html ()
bodyWrapper BWConfig{sessM, currProject, pageTitle, menuItem} child =
  case sessM of
    Nothing -> child
    Just sess -> do
      let currUser = Sessions.getUser (Sessions.user sess)
          sideNav' = currProject & maybe "" \project -> sideNav sess project pageTitle menuItem
      let currUserEmail = CI.original currUser.email

      doctypehtml_ $ do
        head_ $ do
          title_ $ toHtml pageTitle
          meta_ [charset_ "UTF-8"]
          meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
          meta_ [httpEquiv_ "X-UA-Compatible", content_ "ie=edge"]
          -- favicon items
          link_ [rel_ "apple-touch-icon", sizes_ "180x180", href_ "/apple-touch-icon.png"]
          link_ [rel_ "icon", type_ "image/png", sizes_ "32x32", href_ "/favicon-32x32.png"]
          link_ [rel_ "icon", type_ "image/png", sizes_ "16x16", href_ "/favicon-16x16.png"]
          link_ [rel_ "manifest", href_ "/site.webmanifest"]
          link_ [rel_ "mask-icon", href_ "/safari-pinned-tab.svg", term "color" "#5bbad5"]
          meta_ [name_ "msapplication-TileColor", content_ "#da532c"]
          meta_ [name_ "theme-color", content_ "#ffffff"]

          link_ [rel_ "stylesheet", type_ "text/css", href_ "/assets/css/tailwind.min.css"]
          link_ [rel_ "stylesheet", type_ "text/css", href_ "/assets/css/thirdparty/notyf3.min.css"]
          link_ [rel_ "preconnect", href_ "https://fonts.googleapis.com"]
          link_ [rel_ "preconnect", href_ "https://fonts.gstatic.com", crossorigin_ "true"]
          link_ [href_ "https://fonts.googleapis.com/css2?family=Inconsolata&family=Poppins:wght@400;500;600&display=swap", rel_ "stylesheet"]
          link_ [rel_ "stylesheet", href_ "https://unpkg.com/swagger-ui-dist@4.5.0/swagger-ui.css"]
          -- SCRIPTS
          script_ [src_ "https://cdn.jsdelivr.net/npm/echarts@5.4.1/dist/echarts.min.js"] ("" :: Text)
          script_ [src_ "/assets/roma-echarts.js", defer_ "true"] ("" :: Text)
          script_ [src_ "/assets/js/thirdparty/notyf3.min.js", defer_ "true"] ("" :: Text)
          script_ [src_ "/assets/js/thirdparty/htmx1_8_4.min.js", defer_ "true"] ("" :: Text)
          script_ [src_ "/assets/js/thirdparty/_hyperscript_web0_9_5.min.js", defer_ "true"] ("" :: Text)
          script_ [src_ "/assets/js/thirdparty/luxon.min.js", defer_ "true"] ("" :: Text)
          script_ [src_ "/assets/js/thirdparty/popper2_11_4.min.js", defer_ "true"] ("" :: Text)
          script_ [src_ "/assets/js/thirdparty/tippy6_3_7.umd.min.js", defer_ "true"] ("" :: Text)
          script_ [src_ "/assets/js/thirdparty/instantpage5_1_0.js", type_ "module", defer_ "true"] ("" :: Text)
          script_ [src_ "/assets/js/monaco/vs/luxon.min.js", defer_ "true"] ("" :: Text)
          script_ [src_ "/assets/js/monaco/vs/loader.js", defer_ "true"] ("" :: Text)

          -- script_ [src_ "https://cdn.jsdelivr.net/npm/@easepick/core@1.2.0/dist/index.umd.min.js"] ("" :: Text)
          -- script_ [src_ "https://cdn.jsdelivr.net/npm/@easepick/datetime@1.2.0/dist/index.umd.min.js"] ("" :: Text)
          -- script_ [src_ "https://cdn.jsdelivr.net/npm/@easepick/base-plugin@1.2.0/dist/index.umd.min.js"] ("" :: Text)
          -- script_ [src_ "https://cdn.jsdelivr.net/npm/@easepick/range-plugin@1.2.0/dist/index.umd.min.js"] ("" :: Text)
          -- script_ [src_ "https://cdn.jsdelivr.net/npm/@easepick/preset-plugin@1.2.0/dist/index.umd.min.js"] ("" :: Text)
          -- script_ [src_ "https://cdn.jsdelivr.net/npm/@easepick/time-plugin@1.2.0/dist/index.umd.min.js"] ("" :: Text)
          script_ [src_ "https://cdn.jsdelivr.net/npm/@easepick/bundle@1.2.0/dist/index.umd.min.js"] ("" :: Text)

          script_
            [text|
              window.initialCloseSideMenu = localStorage.getItem('close-sidemenu');
              var currentISOTimeStringVar = ((new Date()).toISOString().split(".")[0])+"+00:00";
              document.addEventListener('DOMContentLoaded', function(){ 
                if (window.initialCloseSideMenu == 'true'){
                   document.getElementById('side-nav-menu').classList.add('hidden-side-nav-menu');
                }


                // htmx.config.useTemplateFragments = true
                // htmx.logAll()
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
                    navigator.serviceWorker.register("/sw.js").then(swReg => {}).catch(err => {
                        console.error('Service Worker Error', err);
                    });
                });
              }
            |]
          script_ Charts.chartInit
          script_
            [text|
            // Ortto apitoolkit capture code 
            window.ap3c = window.ap3c || {};
            var ap3c = window.ap3c;
            ap3c.cmd = ap3c.cmd || [];
            ap3c.cmd.push(function() {
                ap3c.init('ZCp34YmuGHeQ46i4YXBpdG9vbGtpdA', 'https://capture-api.eu.autopilotapp.com/');
                ap3c.track({v: 0});
            });
            ap3c.activity = function(act) { ap3c.act = (ap3c.act || []); ap3c.act.push(act); };
            var s, t; s = document.createElement('script'); s.type = 'text/javascript'; s.src = "https://cdneu.net/app.js";
            t = document.getElementsByTagName('script')[0]; t.parentNode.insertBefore(s, t);

            // Track user on dashboard
            ap3c.track({email: "$currUserEmail", skipNonExisting: true});
          |]

        body_ [class_ "text-gray-900"] $ do
          section_ [class_ "flex flex-row h-screen overflow-hidden"] $ do
            sideNav'
            section_ [class_ "grow h-full overflow-y-hidden"] $ do
              navbar currUser
              section_ [class_ "h-full overflow-y-auto"] $ do
                child

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
    $ do
      div_ [class_ "p-2 pb-4 "] $ do
        div_ [class_ "flex mt-2 mb-4"] $ do
          img_ [class_ "p-4", src_ "/assets/svgs/projects.svg"]
          div_ $ do
            strong_ [class_ "block"] $ toHtml $ currProject.title
            small_ [class_ "block text-blue-800"] "Development"
        nav_ [] $ do
          a_ [href_ [text| /p/$pidTxt/settings |], class_ "p-3 flex gap-3 rounded-2xl hover:bg-gray-100"] $ do
            img_ [src_ "/assets/svgs/settings.svg"]
            span_ "Settings"
          a_ [href_ [text| /p/$pidTxt/manage_members |], class_ "p-3 flex gap-3 rounded hover:bg-gray-100"] $ do
            img_ [src_ "/assets/svgs/add_user.svg"]
            span_ "Manage members"
          a_ [class_ "hidden p-3 flex gap-3 rounded hover:bg-gray-100 "] $ do
            img_ [src_ "/assets/svgs/dollar.svg"]
            span_ "Billing and usage"
      div_ [class_ "border-t border-gray-100 p-2"] $ do
        div_ [class_ "flex justify-between content-center items-center py-5 mb-2 "] $ do
          a_ [href_ "/"] $ h3_ [class_ "text-xl"] "Switch projects"
          a_ [class_ "inline-block bg-blue-700 flex pl-3 pr-4 py-2 rounded-xl text-white space-x-2", href_ "/p/new"] $ do
            img_ [class_ "bg-blue-800 p-2 rounded-lg", src_ "/assets/svgs/plus.svg"]
            span_ [class_ "inline-block px-1"] "Add"
        div_ $ do
          div_ [class_ "relative"] $ do
            div_ [class_ "absolute inset-y-0 left-0 pl-4 flex items-center pointer-events-none"] $ do
              img_ [class_ "", src_ "/assets/svgs/search.svg"]
            input_ [class_ "pl-12 w-full text-sm bg-gray-100 rounded-2xl border-0 p-3", placeholder_ "Search Projects"]
          div_ [class_ "space-y-2 py-4 text-sm"] $ do
            projects & mapM_ \project -> do
              a_ [class_ "flex justify-between p-2", href_ $ "/p/" <> project.id.toText] $ do
                div_ [class_ "space-x-3"] $ do
                  img_ [class_ "inline-block", src_ "/assets/svgs/projects.svg"]
                  span_ [class_ "inline-block"] $ toHtml $ project.title
                when (currProject.id == project.id) $ img_ [src_ "/assets/svgs/checkmark_blue.svg"]

sideNav :: Sessions.PersistentSession -> Projects.Project -> Text -> Maybe Text -> Html ()
sideNav sess project pageTitle menuItem = do
  aside_ [class_ "shrink-0 top-0 border-r-2 bg-white border-gray-200 h-screen overflow-hidden transition-all duration-1000 ease-in-out", id_ "side-nav-menu"] $ do
    a_ [href_ "/", class_ "inline-block p-4 h-12"] $ do
      img_
        [ class_ "h-12 sd-hidden"
        , src_ "/assets/svgs/logo.svg"
        ]
      img_
        [ class_ "h-12 w-10 hidden sd-show"
        , src_ "/assets/svgs/logo_mini.svg"
        ]
    div_ [class_ "py-4 px-4 transition-all  duration-1000 ease-in-out", id_ "side-nav-ctx-btn"] $ do
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
        $ do
          div_ [class_ "space-2 grow sd-hidden"] $ do
            strong_ [class_ "block text-slate-900"] $ toHtml $ project.title
            small_ [class_ "block text-slate-900"] "Development"
          div_ $ do
            img_ [src_ "/assets/svgs/up_chevron.svg"]
            img_ [src_ "/assets/svgs/down_chevron.svg"]
      projectsDropDown project (Sessions.getProjects $ Sessions.projects sess)
    nav_ [class_ "mt-4"] $ do
      -- FIXME: reeanable hx-boost hxBoost_ "true"
      menu (project.id) & mapM_ \(mTitle, mUrl, mIcon) -> do
        a_
          [ href_ mUrl
          , class_ $
              "block flex gap-3 px-5 py-3 flex justify-center items-center hover:bg-blue-50 text-slate-800 "
                <> ( if maybe (pageTitle == mTitle) (== mTitle) menuItem
                      then "bg-blue-50 border-l-4 border-blue-700"
                      else ""
                   )
          ]
          $ do
            svg_ [class_ "w-5 h-5 icon text-slate-500", term "data-tippy-placement" "right", term "data-tippy-content" mTitle] $ use_ [href_ $ "/assets/svgs/sprite/sprite.svg" <> mIcon]
            span_ [class_ "grow sd-hidden"] $ toHtml mTitle

navbar :: Users.User -> Html ()
navbar currUser = do
  nav_ [id_ "main-navbar", class_ "sticky z-20 top-0 w-full w-full px-6 py-3 border-b bg-white flex flex-row justify-between"] $ do
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
      $ do
        img_ [class_ "w-4 h-4", src_ "/assets/svgs/hamburger_menu.svg"]
    div_ [class_ "inline-block flex items-center"] $ do
      a_ [class_ "inline-block p-2 px-3 align-middle"] $ img_ [class_ "w-5 h-5", src_ "/assets/svgs/search.svg"]
      a_ [class_ "inline-block border-r-2 p-2 pr-5"] $ img_ [class_ "w-5 h-5", src_ "/assets/svgs/notifications_active.svg"]
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
        $ do
          img_ [class_ "inline-block w-9 h-9 rounded-lg bg-gray-300", src_ (currUser.displayImageUrl)]
          span_ [class_ "inline-block"] $ toHtml $ currUser.firstName <> " " <> currUser.lastName
          img_ [class_ "w-4 h-4 inline-block", src_ "/assets/svgs/down_caret.svg"]

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
        $ do
          -- dropdown mainbody
          a_ [class_ "text-base p-2 flex gap-3 rounded hover:bg-gray-100", href_ "/logout"] $ do
            img_ [src_ "/assets/svgs/add_user.svg"]
            span_ "Logout"
