{-# LANGUAGE GADTs #-}

module Utils (eitherStrToText, GetOrRedirect, redirect, DBField (..)) where

import Database.PostgreSQL.Simple.ToField (ToField)
import Lucid (Html)
import Relude
import Servant

-- | eitherStrToText helps to convert Either String a to Either Text a,
-- to allow using both of them via do notation in the same monad.
eitherStrToText :: Either String a -> Either Text a
eitherStrToText (Left str) = Left $ toText str
eitherStrToText (Right a) = Right a

type GetOrRedirect = '[WithStatus 200 (Html ()), WithStatus 302 (Headers '[Header "Location" Text] NoContent)]

redirect :: Text -> Headers '[Header "Location" Text] NoContent
redirect destination = addHeader destination NoContent

data DBField where
  MkDBField :: ToField a => a -> DBField
