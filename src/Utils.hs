{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Utils (eitherStrToText, GetOrRedirect, redirect, DBField (..), mIcon_, deleteParam, quoteTxt, textToBool) where

import Data.Text (replace)
import Data.Time (ZonedTime)
import Database.PostgreSQL.Simple.ToField (ToField (..))
import Lucid (Html, href_)
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

deleteParam :: Text -> Text -> Text
deleteParam key url = if needle == "" then url else replace needle "" url
 where
  needle = url =~ reg :: Text
  reg = "&" <> key <> "(=[^&]*)?|^" <> key <> "(=[^&]*)?&?" :: Text

quoteTxt :: Text -> Text
quoteTxt a = "'" <> a <> "'"

textToBool :: Text -> Bool
textToBool a = a == "true"
