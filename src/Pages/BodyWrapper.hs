{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Pages.BodyWrapper (bodyWrapper) where

import qualified Data.UUID as UUID
import qualified Data.Vector as Vector
import Lucid
import Lucid.HTMX
import qualified Models.Projects.Projects as Projects
import qualified Models.Users.Sessions as Sessions
import Optics.Operators ((^.))
import Relude
import Text.RawString.QQ

menu :: Projects.ProjectId -> [(Text, Text, Text)]
menu ppid =
  let pid = Projects.projectIdText ppid
   in [ ("Dashboard", "/p/" <> pid <> "/", "/assets/svgs/dashboard.svg"),
        ("Endpoints", "/p/" <> pid <> "/endpoints", "/assets/svgs/dashboard.svg"),
        ("API Keys", "/p/" <> pid <> "/", "/assets/svgs/dashboard.svg")
      ]

bodyWrapper :: Maybe Sessions.PersistentSession -> Maybe Projects.Project -> Text -> Html () -> Html ()
bodyWrapper sessM currProject title child =
  case sessM of
    Nothing -> child
    Just sess -> do
      let currUser = Sessions.getUser (Sessions.user sess)
      doctypehtml_ $ do
        head_ $ do
          title_ $ toHtml title
          link_ [rel_ "stylesheet", type_ "text/css", href_ "/assets/css/tailwind.min.css"]
          script_ [src_ "https://unpkg.com/htmx.org@1.6.1"] ("" :: Text)
          script_ [src_ "https://unpkg.com/hyperscript.org@0.9.3"] ("" :: Text)
        body_ [class_ "text-gray-700"] $ do
          section_ [class_ "flex flex-row"] $ do
            -- Side nav
            case currProject of
              Nothing -> toHtml ""
              Just project -> do
                div_ [class_ "flex flex-row"] $ do
                  aside_ [class_ "w-1/4 border-r-2 bg-white border-gray-200 h-screen fixed"] $ do
                    div_ [class_ "p-4"] $ do
                      img_ [src_ "/assets/svgs/logo.svg"]
                    div_ [class_ "p-4"] $ do
                      a_
                        [ class_ "flex flex-row bg-gray-100 block p-6 rounded-md cursor-pointer",
                          term
                            "_"
                            [r| 
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
                          div_ [class_ "space-2 grow "] $ do
                            strong_ [class_ "block"] $ toHtml $ project ^. #title
                            small_ [class_ "block"] "Development"
                          div_ $ do
                            img_ [src_ "/assets/svgs/up_chevron.svg"]
                            img_ [src_ "/assets/svgs/down_chevron.svg"]
                      projectsDropDown project (Sessions.getProjects $ Sessions.projects sess)
                    nav_ [class_ "mt-4"] $ do
                      menu (project ^. #id)
                        & mapM_
                          ( \(mTitle, mUrl, mIcon) -> do
                              a_ [href_ mUrl, class_ $ "block flex gap-3 px-5 py-3" <> (if title == mTitle then " bg-gray-100 border-l-4 border-blue-700" else "")] $ do
                                img_ [src_ mIcon]
                                span_ [class_ "flex-grow"] $ toHtml mTitle
                          )
                  section_ [class_ "pt-20 p-5 w-3/4 grow absolute right-0"] child   

            -- main body
            section_ [class_ " grow w-full bg-gray-50"] $ do
              nav_ [class_ "fixed grow w-full px-6 py-3 border-b bg-white flex flex-row justify-between"] $ do
                div_ [class_ "flex "] $ do
                  img_ [src_ "/assets/svgs/logo.svg"]
                  img_ [src_ "/assets/svgs/hamburger_menu.svg", class_ "ml-5"]
                div_ [class_ "inline-block flex items-center"] $ do
                  a_ [class_ "inline-block p-2 px-3 align-middle"] $ img_ [src_ "/assets/svgs/search.svg"]
                  a_ [class_ "inline-block border-r-2 p-2 pr-5"] $ img_ [src_ "/assets/svgs/notifications_active.svg"]
                  a_ [class_ "cursor-pointer inline-block space-x-4 pl-4 relative ",
                    term
                      "_"
                      [r| 
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
                    ] $ do
                    img_ [class_ "inline-block w-9 h-9 rounded-lg", src_ (currUser ^. #displayImageUrl)]
                    span_ [class_ "inline-block"] $ toHtml $ currUser ^. #firstName <> " " <> currUser ^. #lastName
                    img_ [class_ "inline-block", src_ "/assets/svgs/down_caret.svg" ]

                    --logout dropdown
                  div_ [term "drop-menu" "true", 
                    class_ "hidden origin-top-left border border-gray-100 w-[10rem] rounded-lg shadow-2xl shadow-indigo-200 z-40 transition transform bg-white p-1 absolute top-14 right-5 ",
                    term
                      "_"
                      [r|
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
                   ] $ do
                    -- dropdown mainbody
                    a_ [class_ "text-base p-2 flex gap-3 rounded hover:bg-gray-100", href_ "/logout"] $ do
                      img_ [src_ "/assets/svgs/add_user.svg"]
                      span_ "Logout"

              -- section_ [class_ "p-6 "] child

projectsDropDown :: Projects.Project -> Vector.Vector Projects.Project -> Html ()
projectsDropDown currProject projects =
  div_
    [ term "data-menu" "true",
      class_ "hidden origin-top-right z-40 transition transform bg-white p-4 absolute w-[20rem] rounded-2xl shadow-2xl shadow-indigo-200",
      term
        "_"
        [r|
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
            strong_ [class_ "block"] $ toHtml $ currProject ^. #title
            small_ [class_ "block text-gray-600"] "Development"
        nav_ [] $ do
          a_ [class_ "p-3 flex gap-3 rounded-2xl bg-gray-100"] $ do
            img_ [src_ "/assets/svgs/settings.svg"]
            span_ "Settings"
          a_ [class_ "p-3 flex gap-3 rounded"] $ do
            img_ [src_ "/assets/svgs/add_user.svg"]
            span_ "Invite a member"
          a_ [class_ "p-3 flex gap-3 rounded"] $ do
            img_ [src_ "/assets/svgs/dollar.svg"]
            span_ "Billing and usage"
      div_ [class_ "border-t border-gray-100 p-2"] $ do
        div_ [class_ "flex justify-between content-center items-center py-5 mb-2 "] $ do
          h3_ [class_ "text-xl"] "Switch projects"
          a_ [class_ "inline-block bg-blue-700 flex pl-3 pr-4 py-2 rounded-xl text-white space-x-2", href_ "/p/new"] $ do
            img_ [class_ "bg-blue-800 p-2 rounded-lg", src_ "/assets/svgs/plus.svg"]
            span_ [class_ "inline-block px-1"] "Add"
        div_ $ do
          div_ [class_ "relative"] $ do
            div_ [class_ "absolute inset-y-0 left-0 pl-4 flex items-center pointer-events-none"] $ do
              img_ [class_ "", src_ "/assets/svgs/search.svg"]
            input_ [class_ "pl-12 w-full text-sm bg-gray-100 rounded-2xl border-0 p-3", placeholder_ "Search Projects"]
          div_ [class_ "space-y-2 py-4 text-sm"] $ do
            projects
              & mapM_
                ( \project -> do
                    a_ [class_ "flex justify-between p-2", href_ ("/p/" <> Projects.projectIdText (project ^. #id))] $ do
                      div_ [class_ "space-x-3"] $ do
                        img_ [class_ "inline-block", src_ "/assets/svgs/projects.svg"]
                        span_ [class_ "inline-block"] $ toHtml $ project ^. #title
                      when (currProject ^. #id == project ^. #id) $ img_ [src_ "/assets/svgs/checkmark_blue.svg"]
                )
