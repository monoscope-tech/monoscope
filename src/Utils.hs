{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Utils (eitherStrToText, GetOrRedirect, redirect, DBField (..)) where

import Data.Time (ZonedTime)
import Database.PostgreSQL.Simple.ToField (ToField (..))
import Lucid (Html)
import Relude
import Servant

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

data DBField = forall a. ToField a => MkDBField a

instance ToField DBField where
  toField (MkDBField a) = toField a

-- Useful Alternative abstractions
(<?>) :: Alternative f => f a -> a -> f a
(<?>) fa def = fa <|> pure def
