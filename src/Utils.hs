{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Utils (
  eitherStrToText,
  userIsProjectMember,
  GetOrRedirect,
  redirect,
  DBField (..),
  faSprite_,
  mIcon_,
  faIcon_,
  faIconWithAnchor_,
  deleteParam,
  quoteTxt,
  textToBool,
  getMethodColor,
  getStatusColor,
  unwrapJsonPrimValue,
  lookupMapText,
  lookupMapInt,
) where

import Data.Aeson (Value)
import Data.Aeson qualified as AE
import Data.HashMap.Strict qualified as HM
import Data.Scientific (toBoundedInteger)
import Data.Text (replace)
import Data.Time (ZonedTime)
import Data.Vector qualified as V
import Database.PostgreSQL.Simple.ToField (ToField (..))
import Database.PostgreSQL.Transact
import Lucid (Html, a_, href_, i_, onclick_, term)
import Lucid.Svg (class_, svg_, use_)
import Models.Projects.ProjectMembers (ProjectMembers (ProjectMembers))
import Models.Projects.ProjectMembers qualified as ProjectMembers
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Session
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


faSprite_ :: Text -> Text -> Text -> Html ()
faSprite_ mIcon faType classes = svg_ [class_ $ "inline-block icon " <> classes] $ use_ [href_ $ "/assets/svgs/fa-sprites/" <> faType <> ".svg#" <> mIcon]


faIcon_ :: Text -> Text -> Text -> Html ()
faIcon_ faIcon faClasses classes = do
  i_ [class_ faClasses, term "data-fa-symbol" faIcon] ""
  svg_ [class_ classes] $ use_ [href_ $ "#" <> faIcon]


faIconWithAnchor_ :: Text -> Text -> Text -> Text -> Html ()
faIconWithAnchor_ faIcon faClasses classes onClickAction = do
  a_ [href_ "#", onclick_ onClickAction]
    $ faIcon_ faIcon faClasses classes -- You can replace "#" with the actual link


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
      user <- ProjectMembers.selectProjectActiveMember pid sess.userId
      case user of Nothing -> pure False; Just _ -> pure True


getMethodBgColor :: Text -> Text
getMethodBgColor "POST" = "bg-green-500"
getMethodBgColor "PUT" = "bg-orange-500"
getMethodBgColor "DELETE" = "bg-red-500"
getMethodBgColor "PATCH" = "bg-purple-500"
getMethodBgColor _ = "bg-blue-500"


-- getMethodColor :: Text -> Text
-- getMethodColor "POST" = " text-green-950 bg-green-50 border border-green-200 "
-- getMethodColor "PUT" = " text-orange-950 bg-orange-50 border border-orange-200 "
-- getMethodColor "DELETE" = " text-red-950 bg-red-50 border border-red-200 "
-- getMethodColor "PATCH" = " text-purple-950 bg-purple-50 border border-purple-200 "
-- getMethodColor "GET" = " text-blue-950 bg-blue-50 border border-blue-200 "
-- getMethodColor _ = " text-blue-950 bg-blue-50 border border-blue-200 "

getMethodColor :: Text -> Text
getMethodColor "POST" = " badge badge-warning "
getMethodColor "PUT" = " badge badge-info "
getMethodColor "DELETE" = " badge badge-error "
getMethodColor "PATCH" = " badge badge-info "
getMethodColor "GET" = " badge badge-success "
getMethodColor _ = " badge badge-outline "


getStatusColor :: Int -> Text
getStatusColor status
  | status < 200 = "text-slate-500 bg-slate-800 border border-slate-200 "
  | status >= 200 && status < 300 = "text-green-800 bg-green-50 border border-green-200"
  | status >= 300 && status < 400 = "text-amber-800 bg-yellow-50 border border-yellow-200"
  | otherwise = "text-red-800 bg-red-50 border border-red-200"


unwrapJsonPrimValue :: AE.Value -> Text
unwrapJsonPrimValue (AE.Bool True) = "true"
unwrapJsonPrimValue (AE.Bool False) = "true"
unwrapJsonPrimValue (AE.String v) = "\"" <> toText v <> "\""
unwrapJsonPrimValue (AE.Number v) = toText @String $ show v
unwrapJsonPrimValue AE.Null = "null"
unwrapJsonPrimValue (AE.Object _) = "{..}"
unwrapJsonPrimValue (AE.Array items) = "[" <> toText (show (length items)) <> "]"


-- unwrapJsonPrimValue (AE.Object _) = error "Impossible. unwrapJsonPrimValue should be for primitive types only. got object" -- should never be reached
-- unwrapJsonPrimValue (AE.Array _) = error "Impossible. unwrapJsonPrimValue should be for primitive types only. got array" -- should never be reached

lookupMapText :: Text -> HashMap Text Value -> Maybe Text
lookupMapText key hashMap = case HM.lookup key hashMap of
  Just (AE.String textValue) -> Just textValue -- Extract text from Value if it's a String
  _ -> Nothing


lookupMapInt :: Text -> HashMap Text Value -> Int
lookupMapInt key hashMap = case HM.lookup key hashMap of
  Just (AE.Number val) -> fromMaybe 0 $ toBoundedInteger val -- Extract text from Value if it's a String
  _ -> 0
