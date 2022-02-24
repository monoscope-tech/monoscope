module Utils (eitherStrToText) where

import Relude

-- | eitherStrToText helps to convert Either String a to Either Text a,
-- to allow using both of them via do notation in the same monad.
eitherStrToText :: Either String a -> Either Text a
eitherStrToText (Left str) = Left $ toText str
eitherStrToText (Right a) = Right a
