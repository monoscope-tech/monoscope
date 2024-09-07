module Pages.HashAssets (hashAssetFile) where

import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax qualified as TH
import Data.Digest.XXHash (xxHash)
import Numeric (showHex)
import Relude

-- adds a version hash to file paths, to force cache invalidation when a new version appears
hashAssetFile :: FilePath -> TH.Q TH.Exp
hashAssetFile path = do
  content <- TH.runIO $ BL.readFile ("static" <> path)
  let hash = fromString $ showHex (xxHash content) ""
  [|$(TH.lift path) <> "?v=" <> $(TH.lift (T.unpack hash))|]
