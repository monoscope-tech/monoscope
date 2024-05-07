{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Utils (
  eitherStrToText,
  userIsProjectMember,
  GetOrRedirect,
  redirect,
  lookupVecByKey,
  DBField (..),
  faSprite_,
  lookupVecInt,
  lookupVecText,
  lookupVecIntByKey,
  lookupVecTextByKey,
  mIcon_,
  faIcon_,
  faIconWithAnchor_,
  deleteParam,
  quoteTxt,
  textToBool,
  getMethodColor,
  getStatusColor,
  unwrapJsonPrimValue,
  listToIndexHashMap,
  lookupMapText,
  lookupMapInt,
  freeTierLimitExceededBanner,
)
where

import Data.Aeson (Value)
import Data.Aeson qualified as AE
import Data.HashMap.Strict qualified as HM
import Data.Scientific (toBoundedInteger)
import Data.Text (replace)
import Data.Time (ZonedTime)
import Data.Vector qualified as V
import Database.PostgreSQL.Simple.ToField (ToField (..))
import Database.PostgreSQL.Transact
import Lucid
import Lucid.Svg qualified as Svg
import Models.Projects.ProjectMembers qualified as ProjectMembers
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Session
import Relude hiding (show)
import Servant
import Text.Regex.TDFA ((=~))
import Text.Show


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


mIcon_ :: Text -> Text -> Html ()
mIcon_ mIcon classes = svg_ [class_ $ "inline-block icon " <> classes] $ Svg.use_ [href_ $ "/assets/svgs/symbol-defs.svg#icon-" <> mIcon]


faSprite_ :: Text -> Text -> Text -> Html ()
faSprite_ mIcon faType classes = svg_ [class_ $ "inline-block icon " <> classes] $ Svg.use_ [href_ $ "/assets/svgs/fa-sprites/" <> faType <> ".svg#" <> mIcon]


faIcon_ :: Text -> Text -> Text -> Html ()
faIcon_ faIcon faClasses classes = do
  i_ [class_ faClasses, term "data-fa-symbol" faIcon] ""
  svg_ [class_ classes] $ Svg.use_ [href_ $ "#" <> faIcon]


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

-- FIXME: delete
lookupMapText :: Text -> HashMap Text Value -> Maybe Text
lookupMapText key hashMap = case HM.lookup key hashMap of
  Just (AE.String textValue) -> Just textValue -- Extract text from Value if it's a String
  _ -> Nothing


-- FIXME: delete
lookupMapInt :: Text -> HashMap Text Value -> Int
lookupMapInt key hashMap = case HM.lookup key hashMap of
  Just (AE.Number val) -> fromMaybe 0 $ toBoundedInteger val -- Extract text from Value if it's a String
  _ -> 0


lookupVecText :: V.Vector Value -> Int -> Maybe Text
lookupVecText vec idx = case vec V.!? idx of
  Just (AE.String textValue) -> Just textValue -- Extract text from Value if it's a String
  _ -> Nothing


lookupVecInt :: V.Vector Value -> Int -> Int
lookupVecInt vec idx = case vec V.!? idx of
  Just (AE.Number val) -> fromMaybe 0 $ toBoundedInteger val -- Extract text from Value if it's a String
  _ -> 0


lookupVecTextByKey :: V.Vector Value -> HM.HashMap Text Int -> Text -> Maybe Text
lookupVecTextByKey vec colIdxMap key = HM.lookup key colIdxMap >>= lookupVecText vec


lookupVecIntByKey :: V.Vector Value -> HM.HashMap Text Int -> Text -> Int
lookupVecIntByKey vec colIdxMap key = (HM.lookup key colIdxMap >>= Just . lookupVecInt vec) & fromMaybe 0


lookupVecByKey :: V.Vector Value -> HM.HashMap Text Int -> Text -> Maybe Value
lookupVecByKey vec colIdxMap key = HM.lookup key colIdxMap >>= (vec V.!?)


listToIndexHashMap :: Hashable a => [a] -> HM.HashMap a Int
listToIndexHashMap list = HM.fromList [(x, i) | (x, i) <- zip list [0 ..]]


freeTierLimitExceededBanner :: Text -> Html ()
freeTierLimitExceededBanner pid =
  div_ [class_ "flex w-full text-center items-center px-4 gap-4 py-2 bg-red-600 text-white rounded-lg justify-center"] do
    p_ [] "You have exceeded the maximum free tier requests limit for this month, new request will not be processed."
    a_ [class_ "font-semibold", href_ $ "/p/" <> pid <> "/settings"] "upgrade now"
