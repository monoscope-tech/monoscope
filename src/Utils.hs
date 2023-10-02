{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Utils (
  eitherStrToText,
  userIsProjectMember,
  userNotMemeberPage,
  GetOrRedirect,
  redirect,
  DBField (..),
  mIcon_,
  deleteParam,
  quoteTxt,
  textToBool,
  getMethodColor,
  getMethodBgColor,
  getStatusColor,
  getStatusBgColor,
) where

import Data.Default (def)
import Data.Text (replace)
import Data.Time (ZonedTime)
import Data.Vector qualified as V
import Database.PostgreSQL.Simple.ToField (ToField (..))

import Database.PostgreSQL.Transact
import Lucid (Html, div_, h3_, href_, p_)
import Lucid.Svg (class_, svg_, use_)
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Session
import Models.Users.Users qualified as Users
import Pages.BodyWrapper (BWConfig (..), bodyWrapper)
import Relude hiding (show)
import Servant

import Text.Regex.TDFA ((=~))
import Prelude (show)


-- Added only for satisfying the tests
instance Eq ZonedTime where
  (==) _ _ = True


-- | eitherStrToText helps to convert Either String a to Either Text a,
-- to allow using both of them via do notation in the same monad.
eitherStrToText :: Either String a -> Either Text a
eitherStrToText (Left str) = Left $ toText str
eitherStrToText (Right a) = Right a


type GetOrRedirect = '[WithStatus 200 (Html ()), WithStatus 302 (Headers '[Header "Location" Text] NoContent)]


redirect :: Text -> Headers '[Header "Location" Text] NoContent
redirect destination = addHeader destination NoContent


data DBField = forall a. (ToField a, Show a) => MkDBField a


instance Show DBField where
  show (MkDBField a) = "MkDBField " ++ show a


instance ToField DBField where
  toField (MkDBField a) = toField a


-- Useful Alternative abstractions
(<?>) :: Alternative f => f a -> a -> f a
(<?>) fa def = fa <|> pure def


mIcon_ :: Text -> Text -> Html ()
mIcon_ mIcon classes = svg_ [class_ $ "inline-block icon " <> classes] $ use_ [href_ $ "/assets/svgs/symbol-defs.svg#icon-" <> mIcon]


deleteParam :: Text -> Text -> Text
deleteParam key url = if needle == "" then url else replace needle "" url
  where
    needle = url =~ reg :: Text
    reg = "&" <> key <> "(=[^&]*)?|^" <> key <> "(=[^&]*)?&?" :: Text


quoteTxt :: Text -> Text
quoteTxt a = "'" <> a <> "'"


textToBool :: Text -> Bool
textToBool a = a == "true"


userIsProjectMember :: Session.PersistentSession -> Projects.ProjectId -> DBT IO Bool
userIsProjectMember sess pid = do
  if sess.isSudo
    then pure True
    else do
      user <- Projects.userByProjectId pid sess.userId
      if V.length user == 0 then pure False else pure True


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


getMethodBgColor :: Text -> Text
getMethodBgColor "POST" = "bg-green-500"
getMethodBgColor "PUT" = "bg-orange-500"
getMethodBgColor "DELETE" = "bg-red-500"
getMethodBgColor "PATCH" = "bg-purple-500"
getMethodBgColor _ = "bg-blue-500"


getMethodColor :: Text -> Text
getMethodColor "POST" = "text-green-500"
getMethodColor "PUT" = "text-orange-500"
getMethodColor "DELETE" = "text-red-500"
getMethodColor "PATCH" = "text-purple-500"
getMethodColor _ = "text-blue-500"


getStatusColor :: Int -> Text
getStatusColor status
  | status < 200 = "text-gray-500"
  | status >= 200 && status < 300 = "text-green-500"
  | status >= 300 && status < 400 = "text-yellow-500"
  | otherwise = "text-red-500"


getStatusBgColor :: Int -> Text
getStatusBgColor status
  | status < 200 = "bg-gray-500"
  | status >= 200 && status < 300 = "bg-green-500"
  | status >= 300 && status < 400 = "bg-yellow-500"
  | otherwise = "bg-red-500"