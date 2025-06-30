module Utils.ChartColors (getColorForValue) where

import Data.Text (Text)
import qualified Data.Text as T


-- | Returns a color code (hex) for a given value (status code, log level, etc.)
getColorForValue :: Text -> Text
getColorForValue val
  | is2xx val = "#1976d2" -- blue
  | is3xx val = "#ff9800" -- orange
  | is4xx val = "#e53935" -- red
  | is5xx val = "#b71c1c" -- dark red
  | isInfo val = "#1976d2" -- blue
  | isUnset val = "#9e9e9e" -- gray
  | otherwise = "#607d8b" -- default: blue-gray
  where
    is2xx v = T.length v == 3 && T.head v == '2'
    is3xx v = T.length v == 3 && T.head v == '3'
    is4xx v = T.length v == 3 && T.head v == '4'
    is5xx v = T.length v == 3 && T.head v == '5'
    isInfo v = T.toUpper v == "INFO"
    isUnset v = T.toUpper v == "UNSET" || T.null v
