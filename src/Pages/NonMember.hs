{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Pages.NonMember (userNotMemeberPage) where

import Data.Default (Default (def))
import Lucid
import Models.Users.Sessions qualified as Session
import Pages.BodyWrapper (BWConfig (..), bodyWrapper)
import Relude


userNotMemeberPage :: Session.PersistentSession -> Html ()
userNotMemeberPage sess = bodyWrapper bwconf forbiddenPage
  where
    bwconf =
      (def :: BWConfig)
        { sessM = Just sess
        , currProject = Nothing
        , pageTitle = "Forbidden"
        }


forbiddenPage :: Html ()
forbiddenPage =
  div_ [class_ "w-full flex justify-center"] do
    div_ [class_ "max-w-24 my-32 rounded-xl border p-8"] do
      h3_ [class_ "text-3xl mb-2 font-bold"] "Forbidden"
      p_ [class_ "max-w-prose text-gray-500"] "Only members of this project can access this page, make sure you are logged in to the right account and try again"
