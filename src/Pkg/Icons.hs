module Pkg.Icons (Icon (..), lookupIcon) where

import Data.HashMap.Strict qualified as HM
import Pkg.Icons.TH (embedIconEntries)
import Relude


data Icon = Icon
  { viewBox :: Text
  , attributes :: [(Text, Text)]
  , body :: Text
  }
  deriving stock (Eq, Show)


lookupIcon :: Text -> Text -> Maybe Icon
lookupIcon kind name = HM.lookup (kind, name) iconTable


iconTable :: HM.HashMap (Text, Text) Icon
iconTable = HM.fromList $ map fromEmbedded embeddedIcons
  where
    fromEmbedded (kind, name, viewBox, attrs, body) =
      ( (toText kind, toText name)
      , Icon
          { viewBox = toText viewBox
          , attributes = map (bimap toText toText) attrs
          , body = toText body
          }
      )


embeddedIcons :: [(String, String, String, [(String, String)], String)]
embeddedIcons =
  $( embedIconEntries
      [ ("regular", "static/public/assets/svgs/fa-sprites/regular.svg")
      , ("solid", "static/public/assets/svgs/fa-sprites/solid.svg")
      ]
   )
