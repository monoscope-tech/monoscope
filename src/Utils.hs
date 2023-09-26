{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Utils (eitherStrToText, GetOrRedirect, redirect, DBField (..), mIcon_, faIcon_, deleteParam, quoteTxt, textToBool, getMethodColor, getStatusColor) where

import Data.Text (replace)
import Data.Time (ZonedTime)
import Database.PostgreSQL.Simple.ToField (ToField (..))
import Lucid (Html, href_, i_, term)
import Lucid.Svg (class_, svg_, use_)
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

faIcon_ :: Text -> Text -> Html ()
faIcon_ faIcon classes = do
  i_ [class_ classes, term "data-fa-symbol" faIcon] ""
  svg_ [] $ use_ [href_ $ "#" <> faIcon]

deleteParam :: Text -> Text -> Text
deleteParam key url = if needle == "" then url else replace needle "" url
  where
    needle = url =~ reg :: Text
    reg = "&" <> key <> "(=[^&]*)?|^" <> key <> "(=[^&]*)?&?" :: Text

quoteTxt :: Text -> Text
quoteTxt a = "'" <> a <> "'"

textToBool :: Text -> Bool
textToBool a = a == "true"

getMethodColor :: Text -> Text
getMethodColor "POST" = "green-500"
getMethodColor "PUT" = "orange-500"
getMethodColor "DELETE" = "red-500"
getMethodColor "PATCH" = "purple-500"
getMethodColor _ = "blue-500"

getStatusColor :: Int -> Text
getStatusColor status
  | status < 200 = "gray-500"
  | status >= 200 && status < 300 = "green-500"
  | status >= 300 && status < 400 = "yellow-500"
  | otherwise = "red-500"
